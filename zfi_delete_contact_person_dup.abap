REPORT zfi_delete_contact_person_dup MESSAGE-ID >0 .


CLASS zclfi_remove_cp_dup DEFINITION FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS construtctor ##CALLED.

    CLASS-METHODS get_filepath
      RETURNING
        VALUE(ev_path) TYPE string .

    METHODS fill_data_from_file
      IMPORTING
        iv_pathfile TYPE string .

    METHODS is_valid_data
      RETURNING
        VALUE(rv_value) TYPE abap_bool .

    METHODS process
      IMPORTING
        !iv_testrun TYPE abap_bool DEFAULT '' .

    METHODS prepare_bg
      IMPORTING
        !iv_testrun  TYPE abap_bool DEFAULT ''
        !iv_pathfile TYPE string .

    METHODS process_bg
      IMPORTING
        !iv_testrun TYPE abap_bool DEFAULT ''
        !iv_indxkey TYPE indx-srtfd .

    METHODS log_add
      IMPORTING
        !is_log TYPE bapiret2  OPTIONAL
        !it_log TYPE bapiret2_t OPTIONAL .

    METHODS log_save .

    METHODS log_get
      RETURNING
        VALUE(rt_value) TYPE bapiret2_t ##CALLED .

    METHODS log_display .

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_file,
        partner1 TYPE bus050dfld-partner1,
        partner2 TYPE bus050dfld-partner2,
      END OF ty_file,
      tab_file TYPE STANDARD TABLE OF ty_file,

      BEGIN OF ty_excel,
        a TYPE string,
        b TYPE string,
      END OF ty_excel,
      tab_excel TYPE STANDARD TABLE OF ty_excel.

    CONSTANTS:
      gc_object    TYPE balobj_d  VALUE 'ZFI',
      gc_subobject TYPE balsubobj VALUE 'ZFI_DEL_CP',
      gc_c_table   TYPE char2 VALUE '·T',
      gc_c_range   TYPE char2 VALUE '·R'.

    DATA:
      go_log      TYPE REF TO zcls_fi_ic_logger,
      gt_range_bp TYPE RANGE OF but050-partner1,
      gt_data     TYPE zclfi_remove_cp_dup=>tab_file,
      gt_messages TYPE bapiret2_t.

    METHODS memory_export
      RETURNING
        VALUE(rv_value) TYPE indx-srtfd .

    METHODS memory_import
      IMPORTING
        iv_indxkey TYPE indx-srtfd .

    METHODS memory_delete
      IMPORTING
        iv_indxkey TYPE indx-srtfd .

    METHODS get_jobname
      RETURNING
        VALUE(rv_value) TYPE tbtcjob-jobname .

ENDCLASS.



CLASS zclfi_remove_cp_dup IMPLEMENTATION.


  METHOD construtctor .

    CLEAR:
      me->gt_range_bp, me->gt_data, me->gt_messages, me->go_log .

    me->go_log = NEW zcls_fi_ic_logger( iv_object     = me->gc_object
                                        iv_sub_object = me->gc_subobject ) .

  ENDMETHOD .


  METHOD get_filepath.

    DATA:
      lt_filetable TYPE filetable,
      lv_rc        TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      CHANGING
        file_table              = lt_filetable
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4 ) .

    IF ( sy-subrc EQ 0 ) .

      READ TABLE lt_filetable INDEX 1 INTO DATA(ls_filetable).
      IF ( sy-subrc EQ 0 ) .
        ev_path = ls_filetable-filename.
      ENDIF.

    ENDIF .

  ENDMETHOD.


  METHOD fill_data_from_file .


    DATA:
      lv_excel_data TYPE xstring.

    DATA:
      lv_filelength  TYPE i,
      lt_binary_data TYPE STANDARD TABLE OF x255 WITH NON-UNIQUE DEFAULT KEY,
      lv_filename    TYPE string,
      lo_excel_ref   TYPE REF TO cl_fdt_xl_spreadsheet.

    FIELD-SYMBOLS:
      <fs_data> TYPE zclfi_remove_cp_dup=>tab_excel .


    IF ( me->go_log IS NOT BOUND ) .
      me->go_log = NEW zcls_fi_ic_logger( iv_object     = me->gc_object
                                          iv_sub_object = me->gc_subobject ) .
    ENDIF .

    MOVE iv_pathfile TO lv_filename.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = lv_filename
        filetype                = 'BIN'         " We are basically working with zipped directories --> force binary read
      IMPORTING
        filelength              = lv_filelength
      CHANGING
        data_tab                = lt_binary_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        OTHERS                  = 0 ) .
*        header_not_allowed      = 9
*        separator_not_allowed   = 10
*        header_too_long         = 11
*        unknown_dp_error        = 12
*        access_denied           = 13
*        dp_out_of_memory        = 14
*        disk_full               = 15
*        dp_timeout              = 16
*        not_supported_by_gui    = 17
*        error_no_gui            = 18
*        OTHERS                  = 19 ) .

    IF ( sy-subrc EQ 0 ) .

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_filelength
        IMPORTING
          buffer       = lv_excel_data
        TABLES
          binary_tab   = lt_binary_data.

      TRY .
          lo_excel_ref = NEW cl_fdt_xl_spreadsheet( document_name = lv_filename
                                                    xdocument     = lv_excel_data ) .
        CATCH cx_fdt_excel_core ##NO_HANDLER .

      ENDTRY .

      TRY .
          " Get List of Worksheets
          lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = DATA(lt_worksheets) ) .
        CATCH cx_sy_ref_is_initial ##NO_HANDLER .
      ENDTRY .

      DATA(lo_data_ref) =
        lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( VALUE #( lt_worksheets[ 1 ] OPTIONAL ) )  .

      IF ( lo_data_ref IS NOT INITIAL ) .
        "now you have excel work sheet data in dyanmic internal table
        ASSIGN lo_data_ref->* TO <fs_data> .
      ENDIF .

      IF ( lines( <fs_data>[] ) GT 0 ) .

        me->gt_data =
          VALUE #( FOR <fs_line> IN <fs_data> ( partner1 = |{ <fs_line>-a ALPHA = IN }|
                                                partner2 = |{ <fs_line>-b ALPHA = IN }|  ) ) .

        SORT me->gt_data ASCENDING BY partner1 .
        DELETE ADJACENT DUPLICATES FROM me->gt_data COMPARING ALL FIELDS .

        me->gt_range_bp =
          VALUE #( FOR <fs_line> IN <fs_data> ( sign   = rsmds_c_sign-including
                                                option = rsmds_c_option-equal
                                                low    = CONV but050-partner1( <fs_line>-a ) ) ) .

        SORT me->gt_range_bp ASCENDING BY low .
        DELETE ADJACENT DUPLICATES FROM me->gt_range_bp COMPARING low .

      ENDIF .

    ENDIF .

  ENDMETHOD .


  METHOD is_valid_data .

    IF ( lines( me->gt_data ) GT 0 ) .
      rv_value = abap_on .
    ELSE .

      rv_value = abap_off .

      me->log_add( is_log =  VALUE #( type       = if_xo_const_message=>error
                                      id         = '>0'
                                      number     = 0
                                      message_v1 = 'Data not found' ) ) .

    ENDIF .

  ENDMETHOD .


  METHOD process .

    TYPES:
      BEGIN OF ty_but050,
        relnr     TYPE but050-relnr,
        partner1  TYPE but050-partner1,
        partner2  TYPE but050-partner2,
        date_to   TYPE but050-date_to,
        date_from TYPE but050-date_from,
        reltyp    TYPE but050-reltyp,
        dftval    TYPE but050-dftval,
      END OF ty_but050,
      tab_but050 TYPE STANDARD TABLE OF ty_but050.

    DATA:
      lt_but050 TYPE tab_but050,
      lt_return TYPE bapiret2_t.


    IF ( lines( me->gt_data ) GT 0 ) AND
       ( lines( me->gt_range_bp ) GT 0 )  .

      " Get BP Relationship Number
      SELECT relnr, partner1, partner2, date_to, date_from, reltyp, dftval
        INTO TABLE @lt_but050
        FROM but050
       WHERE partner1 IN @me->gt_range_bp .

      LOOP AT me->gt_data INTO DATA(ls_data) .

        " Flag message test
        IF ( iv_testrun EQ abap_true ) .
          me->log_add( is_log =  VALUE #( type       = if_xo_const_message=>info
                                          id         = '>0'
                                          number     = 0
                                          message_v1 = '(TEST Mode) BP'
                                          message_v2 = |'{ ls_data-partner1 }'| ) ) .
        ENDIF .

        " Check if there is a Relationship Category
        ASSIGN lt_but050[ partner1 = ls_data-partner1
                          partner2 = ls_data-partner2 ]
        TO FIELD-SYMBOL(<fs_but050>) .

        IF ( <fs_but050> IS ASSIGNED ) .

          CALL FUNCTION 'BAPI_BUPR_RELATIONSHIP_REMOVE'
            EXPORTING
              businesspartner1     = ls_data-partner1
              businesspartner2     = ls_data-partner2
              relationshipcategory = <fs_but050>-reltyp
              validfromdate        = <fs_but050>-date_from
              validuntildate       = <fs_but050>-date_to
            TABLES
              return               = lt_return.

          IF ( line_exists( lt_return[ type = if_xo_const_message=>error ] ) ) .

            me->log_add( it_log = VALUE #( FOR l IN lt_return ( l ) ) ) .

          ELSE .

            IF ( lines( lt_return ) GT 0 ) .
              me->log_add( it_log = VALUE #( FOR l IN lt_return ( l ) ) ) .
            ELSE .
              me->log_add( is_log =  VALUE #( type       = if_xo_const_message=>success
                                              id         = '>0'
                                              number     = 0
                                              message_v1 = |'BP' { CONV symsgv( ls_data-partner1 ) }|
                                              message_v2 = |and BP CP { ls_data-partner2 ALPHA = OUT }|
                                              message_v3 = 'has been updated successfully.' ) ) .
*                                             message_v3 = 'has been updated as successful.' ) ) .
            ENDIF .

            IF ( iv_testrun EQ abap_false ) .
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .
            ENDIF .

          ENDIF .

          UNASSIGN <fs_but050> .

        ELSE .
          me->log_add( is_log =  VALUE #( type       = if_xo_const_message=>warning
                                          id         = '>0'
                                          number     = 0
                                          message_v1 = 'BP'
                                          message_v2 = |'{ ls_data-partner1 }'|
                                          message_v3 = 'is not valid.' ) ) .
        ENDIF .


      ENDLOOP .

    ELSE .
      me->log_add( is_log =  VALUE #( type       = if_xo_const_message=>warning
                                      id         = '>0'
                                      number     = 0
                                      message_v1 = 'There are no valid data.' ) ) .

    ENDIF .

    IF ( me->go_log IS BOUND ) .
      me->go_log->persist_data( ).
    ENDIF .

  ENDMETHOD .


  METHOD prepare_bg .

    DATA:
      lv_count TYPE tbtcjob-jobcount,
      lv_debug TYPE char1 VALUE ''.

    IF ( me->go_log IS BOUND ) .

      DATA(lv_indxkey) = me->memory_export( ) .
      DATA(lv_jobname) = me->get_jobname( ) .

      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          jobname          = lv_jobname
        IMPORTING
          jobcount         = lv_count
        EXCEPTIONS
          cant_create_job  = 1
          invalid_job_data = 2
          jobname_missing  = 3
          OTHERS           = 4.

      IF ( sy-subrc EQ 0 ) .

        SUBMIT  zfi_delete_contact_person_dup
         WITH    p_flpath   = iv_pathfile
         WITH    p_test     = iv_testrun
         WITH    p_key      = lv_indxkey
         WITH    p_debug    = lv_debug
         VIA JOB lv_jobname
         NUMBER  lv_count
         AND RETURN .

        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = lv_count
            jobname              = lv_jobname
            sdlstrtdt            = sy-datum
            sdlstrttm            = sy-uzeit
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            invalid_target       = 8
            OTHERS               = 9.

        IF ( sy-subrc EQ 0 ) .

          me->log_add( is_log =  VALUE #( type       = if_xo_const_message=>info
                                          id         = '>0'
                                          number     = 0
                                          message_v1 = 'The records will be processed in the background.' ) ) .

        ELSE .
          me->log_add( is_log =  VALUE #( type       = if_xo_const_message=>error
                                          id         = '>0'
                                          number     = 0
                                          message_v1 = 'The schedule job will not be created.' ) ) .
        ENDIF.

        me->log_add( is_log =  VALUE #( type       = if_xo_const_message=>info
                                        id         = '>0'
                                        number     = 0
                                        message_v1 = 'File:'
                                        message_v2 = CONV #( iv_pathfile ) ) ) .

        me->log_add( is_log =  VALUE #( type       = if_xo_const_message=>info
                                        id         = '>0'
                                        number     = 0
                                        message_v1 = 'Job:'
                                        message_v2 = CONV #( lv_jobname ) ) ) .
      ELSE .
        me->log_add( is_log =  VALUE #( type       = if_xo_const_message=>error
                                        id         = '>0'
                                        number     = 0
                                        message_v1 = 'The schedule job will not be created.' ) ) .
      ENDIF .

    ENDIF .


  ENDMETHOD .


  METHOD process_bg .

    IF ( iv_indxkey IS NOT INITIAL ) .

      IF ( me->go_log IS NOT BOUND ) .
        me->go_log = NEW zcls_fi_ic_logger( iv_object     = me->gc_object
                                            iv_sub_object = me->gc_subobject )  .
      ENDIF .


      IF ( me->go_log IS BOUND ) .

        me->log_add( is_log =  VALUE #( type       = if_xo_const_message=>info
                                        id         = '>0'
                                        number     = 0
                                        message_v1 = 'Processing log of the background execution.' ) ) .

        me->memory_import( iv_indxkey = iv_indxkey ) .

        me->process( iv_testrun = iv_testrun ) .

        me->memory_delete( iv_indxkey = iv_indxkey ) .

      ENDIF .

    ENDIF .

  ENDMETHOD .


  METHOD log_add .

    IF ( is_log IS NOT INITIAL ) .
      APPEND is_log TO me->gt_messages .
    ENDIF.

    IF ( lines( it_log ) GT 0 ) .
      APPEND LINES OF it_log TO me->gt_messages .
    ENDIF .

    " Application log
    IF ( me->go_log IS BOUND ) .

      IF ( is_log IS NOT INITIAL ) .
        me->go_log->log_message( is_message = is_log ) .
      ENDIF.

      LOOP AT it_log INTO DATA(ls_log) .
        me->go_log->log_message( is_message = ls_log ) .
      ENDLOOP .

    ENDIF .

  ENDMETHOD .


  METHOD log_save .

    IF ( me->go_log IS BOUND ) .
      me->go_log->persist_data( ).
    ENDIF .

  ENDMETHOD .


  METHOD log_get ##NEEDED .
  ENDMETHOD .


  METHOD log_display .

    IF ( lines( me->gt_messages ) GT 0 ) .

      CALL FUNCTION 'UDM_MESSAGE_SHOW'
        EXPORTING
          et_message = me->gt_messages
*        IMPORTING
*         ex_error   =
        .

    ENDIF .

  ENDMETHOD .


  METHOD memory_export .

    DATA:
      lt_data TYPE zclfi_remove_cp_dup=>tab_file,
      lr_data TYPE RANGE OF but050-partner1.

    APPEND LINES OF me->gt_data     TO lt_data .
    APPEND LINES OF me->gt_range_bp TO lr_data .

    rv_value = |ZFI·{ sy-datum }·{ sy-uzeit }| .
    DATA(lv_key_table) = CONV indx-srtfd( |{ rv_value }{ me->gc_c_table }| ) .
    DATA(lv_key_range) = CONV indx-srtfd( |{ rv_value }{ me->gc_c_range }| ) .

    EXPORT lt_data FROM lt_data
    TO DATABASE indx(za) ID lv_key_table .

    EXPORT lr_data FROM lr_data
    TO DATABASE indx(za) ID lv_key_range .

  ENDMETHOD .


  METHOD memory_import .

    DATA:
      lt_data TYPE zclfi_remove_cp_dup=>tab_file,
      lr_data TYPE RANGE OF but050-partner1.

    DATA(lv_key_table) = CONV indx-srtfd( |{ iv_indxkey }{ me->gc_c_table }| ) .
    DATA(lv_key_range) = CONV indx-srtfd( |{ iv_indxkey }{ me->gc_c_range }| ) .

    IMPORT lt_data TO lt_data FROM DATABASE indx(za) ID lv_key_table.
    IF ( lines( lt_data ) GT 0 ) .
      me->gt_data = lt_data .
    ENDIF .

    IMPORT lr_data TO lr_data FROM DATABASE indx(za) ID lv_key_range.
    IF ( lines( lr_data ) GT 0 ) .
      me->gt_range_bp = lr_data .
    ENDIF .

  ENDMETHOD .


  METHOD memory_delete .

    DATA(lv_key_table) = CONV indx-srtfd( |{ iv_indxkey }{ me->gc_c_table }| ) .
    DATA(lv_key_range) = CONV indx-srtfd( |{ iv_indxkey }{ me->gc_c_range }| ) .

    DELETE FROM DATABASE indx(za) ID lv_key_table.
    DELETE FROM DATABASE indx(za) ID lv_key_range.

  ENDMETHOD .


  METHOD get_jobname .

    CONSTANTS:
      lc_jobname TYPE tbtcjob-jobname VALUE 'ZFI'.

    CLEAR rv_value .

    rv_value = |{ lc_jobname }-| &&
               |{ sy-datum+6(2) }·{ sy-datum+4(2) }·{ sy-datum(4) }-| &&
               |{ sy-uzeit(2) }·{ sy-uzeit+2(2) }·{ sy-uzeit+4(2) }| .

  ENDMETHOD .


ENDCLASS.

DATA:
  gv_exit TYPE char1 ##NEEDED .

SELECTION-SCREEN BEGIN OF BLOCK blc01 WITH FRAME TITLE text-001.
PARAMETERS:
  p_flpath TYPE string LOWER CASE OBLIGATORY,
  p_test   TYPE char1 AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN BEGIN OF BLOCK blc02 WITH FRAME TITLE text-002.
PARAMETERS:
  p_backg TYPE char1 AS CHECKBOX,
  p_debug TYPE char1  NO-DISPLAY,
  p_key   TYPE  indx-srtfd NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK blc02 .

SELECTION-SCREEN END OF BLOCK blc01 .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_flpath.
  p_flpath = zclfi_remove_cp_dup=>get_filepath( ).

INITIALIZATION.

START-OF-SELECTION.

  IF ( p_debug EQ abap_true ) . DO . IF ( gv_exit EQ abap_true ). EXIT. ENDIF . ENDDO . ENDIF .

  DATA(go_obj) = NEW zclfi_remove_cp_dup( ) ##NEEDED .

  IF ( go_obj IS BOUND ) .

    IF ( p_flpath IS NOT INITIAL ) .

      IF ( p_key IS INITIAL ) . " Processamento em background

        go_obj->fill_data_from_file( iv_pathfile = p_flpath ) .

        CASE p_backg .

          WHEN abap_false .

            IF ( go_obj->is_valid_data( ) EQ abap_true ) .
              go_obj->process( iv_testrun = p_test ) .
            ELSE .
              go_obj->log_save( ) .
            ENDIF .

          WHEN abap_true .

            go_obj->prepare_bg( iv_testrun  = p_test
                                iv_pathfile = p_flpath ) .

          WHEN OTHERS .

        ENDCASE .

        go_obj->log_display( ) .

      ELSE .
        " background processment
        go_obj->process_bg( iv_testrun = p_test
                            iv_indxkey = p_key ) .
      ENDIF .

    ENDIF .

  ENDIF .


END-OF-SELECTION .

  FREE go_obj .
