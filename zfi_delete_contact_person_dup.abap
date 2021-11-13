REPORT zfi_delete_contact_person_dup MESSAGE-ID >0 .

*----------------------------------------------------------------------*
*       CLASS lcl_local DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zclfi_remove_cp_dup DEFINITION FINAL CREATE PUBLIC .


  PUBLIC SECTION.

    METHODS construtctor .

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

    METHODS log_add
      IMPORTING
        !is_log TYPE bapiret2  OPTIONAL
        !it_log TYPE bapiret2_t OPTIONAL .

    METHODS log_save .

    METHODS log_get
      RETURNING
        VALUE(rt_value) TYPE bapiret2_t .

    METHODS log_display .


*    METHODS bg_execute .
*
*    METHODS bg_execute_server
*      IMPORTING
*        !iv_file  TYPE string
*        !iv_debug TYPE abap_bool DEFAULT '' .
*
*    METHODS bg_execute_set_data .
*
*    METHODS bg_execute_get_data .
*
*    METHODS check_diretorio
*      IMPORTING
*        !iv_dir         TYPE string
*      RETURNING
*        VALUE(rv_value) TYPE abap_bool .
*
*    METHODS upload_file
*      IMPORTING
*        !iv_file        TYPE string
*        !iv_dir         TYPE string
*      RETURNING
*        VALUE(rv_value) TYPE string .
*
*    METHODS check_arquivo
*      IMPORTING
*        !iv_dir         TYPE string
*      RETURNING
*        VALUE(rv_value) TYPE abap_bool .
*
*    METHODS delete_file
*      IMPORTING
*        VALUE(iv_filepath) TYPE string .
*
*    METHODS get_filename
*      IMPORTING
*        VALUE(iv_file) TYPE string
*      RETURNING
*        VALUE(rv_file) TYPE string .

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_file,
        partner1 TYPE bus050dfld-partner1,
        partner2 TYPE bus050dfld-partner2,
      END OF ty_file,
      tab_file TYPE TABLE OF ty_file,

      BEGIN OF ty_excel,
        a TYPE string,
        b TYPE string,
      END OF ty_excel,
      tab_excel TYPE TABLE OF ty_excel.

    CONSTANTS:
      gc_object    TYPE balobj_d  VALUE 'ZFI',
      gc_subobject TYPE balsubobj VALUE 'ZFI_DEL_CP'.

    DATA:
      go_log      TYPE REF TO zcls_fi_ic_logger,
      gt_range_bp TYPE RANGE OF but050-partner1,
      gt_data     TYPE zclfi_remove_cp_dup=>tab_file,
      gt_messages TYPE bapiret2_t,
      gv_handle   TYPE c.

*    CLASS-METHODS get_jobname
*      RETURNING
*        VALUE(rv_value) TYPE tbtcjob-jobname .

ENDCLASS.



CLASS zclfi_remove_cp_dup IMPLEMENTATION.


  METHOD construtctor .

    CLEAR:
      me->gt_range_bp, me->gt_data, me->gv_handle, me->gt_messages, me->go_log .

    BREAK-POINT .

    me->go_log = NEW zcls_fi_ic_logger( iv_object     = me->gc_object
                                        iv_sub_object = me->gc_subobject ) .

  ENDMETHOD .


  METHOD get_filepath.

    DATA: lt_filetable TYPE filetable,
          lv_rc        TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      CHANGING
        file_table              = lt_filetable
        rc                      = lv_rc  ##FM_SUBRC_OK
*      EXCEPTIONS
*        file_open_dialog_failed = 1
*        cntl_error              = 2
*        error_no_gui            = 3
*        not_supported_by_gui    = 4
    ).

    READ TABLE lt_filetable INDEX 1 INTO DATA(ls_filetable).
    IF sy-subrc EQ 0.
      ev_path = ls_filetable-filename.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD fill_data_from_file .


    DATA:
      lv_excel_data TYPE xstring.

    DATA:
      lv_filelength   TYPE i,
      lt_binary_data  TYPE STANDARD TABLE OF x255 WITH NON-UNIQUE DEFAULT KEY,
      ls_binary_data  LIKE LINE OF lt_binary_data,
      lv_filename     TYPE string,
      lv_errormessage TYPE string.

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
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ) .
    IF sy-subrc <> 0.
    ENDIF.

*--------------------------------------------------------------------*
* Binary data needs to be provided as XSTRING for further processing
*--------------------------------------------------------------------*
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_filelength
      IMPORTING
        buffer       = lv_excel_data
      TABLES
        binary_tab   = lt_binary_data.


*    lv_excel_data = read_from_local_file( i_filename ).

    FIELD-SYMBOLS:
      <fs_data> TYPE zclfi_remove_cp_dup=>tab_excel .

    DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .

    TRY .
        lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
*                                document_name = CONV #( 'C:\temp\test.xlsx' )
                                document_name = lv_filename
*                              xdocument     = lv_headerxstring ) .
                                xdocument     = lv_excel_data ) .
      CATCH cx_fdt_excel_core.
        "Implement suitable error handling here
    ENDTRY .

    TRY .
        " Get List of Worksheets
        lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
          IMPORTING
            worksheet_names = DATA(lt_worksheets) ).

      CATCH cx_sy_ref_is_initial .
        BREAK-POINT .
    ENDTRY .

    IF NOT lt_worksheets IS INITIAL.
      READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX 1.

      DATA(lo_data_ref) =
       lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_woksheetname )  .

      "now you have excel work sheet data in dyanmic internal table
      ASSIGN lo_data_ref->* TO <fs_data> .

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

    ENDIF.

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
      tab_but050 TYPE TABLE OF ty_but050.

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
                                              message_v1 = 'BP'
                                              message_v2 = CONV #( ls_data-partner1 )
                                              message_v3 = 'has been updated as successful.' ) ) .
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

      IF ( iv_testrun EQ abap_false ) .
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .
      ENDIF.

      IF ( me->go_log IS BOUND ) .
        me->go_log->persist_data( ).
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


  METHOD log_get .
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


*  METHOD add_log .
*
*    IF ( is_line IS NOT INITIAL ) .
*      APPEND is_line TO me->gt_messages .
*    ENDIF .
*
*  ENDMETHOD .
*
*
*  METHOD bg_execute .
*
**    DATA:
**      lv_count   TYPE tbtcjob-jobcount,
**      lv_jobname TYPE tbtcjob-jobname.
**
**    lv_jobname = zclsd_mr21_create_mass=>get_jobname( ) .
**
**
**    CALL FUNCTION 'JOB_OPEN'
**      EXPORTING
**        jobname          = lv_jobname
**      IMPORTING
**        jobcount         = lv_count
**      EXCEPTIONS
**        cant_create_job  = 1
**        invalid_job_data = 2
**        jobname_missing  = 3
**        OTHERS           = 4.
**    IF sy-subrc IS NOT INITIAL.
**    ENDIF.
**
**    me->bg_execute_set_data( it_params = it_shdb_params ).
**
**    SUBMIT  zsd_mr21_create_mass_job
**    VIA JOB lv_jobname
**    NUMBER  lv_count
**    AND RETURN .
**
**    CALL FUNCTION 'JOB_CLOSE'
**      EXPORTING
**        jobcount             = lv_count
***       jobname              = gc_jobname
**        jobname              = lv_jobname
**        sdlstrtdt            = sy-datum
**        sdlstrttm            = sy-uzeit
**      EXCEPTIONS
**        cant_start_immediate = 1
**        invalid_startdate    = 2
**        jobname_missing      = 3
**        job_close_failed     = 4
**        job_nosteps          = 5
**        job_notex            = 6
**        lock_failed          = 7
**        invalid_target       = 8
**        invalid_time_zone    = 9
**        OTHERS               = 10.
**    IF sy-subrc IS NOT INITIAL.
**    ENDIF.
*
*
*  ENDMETHOD .
*
*
*  METHOD bg_execute_get_data .
*
**    DATA:
**      it_params TYPE zsd_excel_data_mr21_tt .
**
**    rt_value = zclsd_mr21_create_mass=>gt_params .
**
**    IMPORT it_params TO it_params FROM SHARED MEMORY indx(xy)
**    CLIENT sy-mandt ID 'IT_PARAMS'.
**
**    zclca_memory=>get_data( EXPORTING iv_id = 'IT_PARAMS'  IMPORTING ev_data = it_params ).
**
**    IF ( lines( it_params ) GT 0 ) .
**      rt_value = it_params .
**    ENDIF .
*
*  ENDMETHOD .
*
*
*  METHOD bg_execute_server .
*
**    DATA:
**      lv_count   TYPE tbtcjob-jobcount,
**      lv_jobname TYPE tbtcjob-jobname.
**
**    lv_jobname = zclsd_mr21_create_mass=>get_jobname( ) .
**
**    CALL FUNCTION 'JOB_OPEN'
**      EXPORTING
**        jobname          = lv_jobname
**      IMPORTING
**        jobcount         = lv_count
**      EXCEPTIONS
**        cant_create_job  = 1
**        invalid_job_data = 2
**        jobname_missing  = 3
**        OTHERS           = 4.
**    IF sy-subrc IS NOT INITIAL.
**    ENDIF.
**
**    SUBMIT  zsd_mr21_create_mass_cockpit
**    WITH    p_fileap = iv_file
**    WITH    p_debug  = iv_debug
**    VIA JOB lv_jobname
**    NUMBER  lv_count
**    AND RETURN .
**
**    CALL FUNCTION 'JOB_CLOSE'
**      EXPORTING
**        jobcount             = lv_count
***       jobname              = gc_jobname
**        jobname              = lv_jobname
**        sdlstrtdt            = sy-datum
**        sdlstrttm            = sy-uzeit
**      EXCEPTIONS
**        cant_start_immediate = 1
**        invalid_startdate    = 2
**        jobname_missing      = 3
**        job_close_failed     = 4
**        job_nosteps          = 5
**        job_notex            = 6
**        lock_failed          = 7
**        invalid_target       = 8
**        invalid_time_zone    = 9
**        OTHERS               = 10.
**    IF sy-subrc IS NOT INITIAL.
**    ENDIF.
*
*
*  ENDMETHOD .
*
*
*  METHOD bg_execute_set_data .
*
**    CLEAR zclsd_mr21_create_mass=>gt_params .
**    zclsd_mr21_create_mass=>gt_params = it_params .
**
**    zclca_memory=>set_data( iv_id = 'IT_PARAMS' iv_data = it_params ).
*
*  ENDMETHOD .
*
*
*  METHOD check_arquivo .
*
*    CLEAR rv_value .
*
*    IF ( iv_dir IS NOT INITIAL ) .
*
*      OPEN DATASET iv_dir FOR INPUT IN BINARY MODE .
*
*      IF ( sy-subrc EQ 0 ) .
*        rv_value = abap_true .
*        CLOSE DATASET iv_dir .
*      ENDIF .
*
*    ENDIF .
*
*  ENDMETHOD .
*
*
*  METHOD check_diretorio .
*
*    CLEAR rv_value .
*
*    IF ( iv_dir IS NOT INITIAL ) .
*
*      OPEN DATASET iv_dir FOR INPUT IN BINARY MODE .
*
*      IF ( sy-subrc EQ 0 ) .
*        rv_value = abap_true .
*        CLOSE DATASET iv_dir .
*      ENDIF .
*
*    ENDIF .
*
*  ENDMETHOD .
*
*
*  METHOD delete_file .
*
*    IF ( iv_filepath IS NOT INITIAL ) .
*      DELETE DATASET iv_filepath .
*    ENDIF .
*
*  ENDMETHOD .
*
*
*
*
*  METHOD fill_struc_from_file.
*
*    CONSTANTS gc_cont TYPE char04 VALUE 'CONT' ##NO_TEXT.
*    CONSTANTS gc_x TYPE c VALUE 'X' ##NO_TEXT.
*    CONSTANTS gc_prefix TYPE char07 VALUE 'FILE://' ##NO_TEXT.
*    CONSTANTS gc_xlscont TYPE char15 VALUE 'EXCEL CONTAINER' ##NO_TEXT.
*    CONSTANTS gc_excel TYPE char05 VALUE 'Excel' ##NO_TEXT.
*    CONSTANTS gc_i TYPE c VALUE 'I' ##NO_TEXT.
*    CONSTANTS gc_test TYPE char04 VALUE 'Test' ##NO_TEXT.
*
*    DATA:
*      lo_cont    TYPE REF TO cl_gui_custom_container,
*      lo_control TYPE REF TO i_oi_container_control,
*      lo_documnt TYPE REF TO i_oi_document_proxy,
*      lo_spsheet TYPE REF TO i_oi_spreadsheet,
*      lo_error   TYPE REF TO i_oi_error.
*
*    DATA:
*      lv_doc_url TYPE c LENGTH 256,
*      lt_sheets  TYPE soi_sheets_table,
*      lt_data    TYPE soi_generic_table,
*      lr_ranges  TYPE soi_range_list.
*
*
*    CALL METHOD c_oi_container_control_creator=>get_container_control
*      IMPORTING
*        control = lo_control
*        error   = lo_error.
*
*    CREATE OBJECT lo_cont
*      EXPORTING
*        container_name = gc_cont ##FM_SUBRC_OK.
**      EXCEPTIONS
**        cntl_error                  = 1
**        cntl_system_error           = 2
**        create_error                = 3
**        lifetime_error              = 4
**        lifetime_dynpro_dynpro_link = 5
**        OTHERS                      = 6  ##FM_SUBRC_OK.
*
*    CALL METHOD lo_control->init_control
*      EXPORTING
*        inplace_enabled     = gc_x
*        r3_application_name = gc_xlscont
*        parent              = lo_cont
*      IMPORTING
*        error               = lo_error ##FM_SUBRC_OK.
**      EXCEPTIONS
**        javabeannotsupported = 1
**        OTHERS               = 2.
*
*    CALL METHOD lo_control->get_document_proxy
*      EXPORTING
*        document_type  = soi_doctype_excel_sheet
*      IMPORTING
*        document_proxy = lo_documnt
*        error          = lo_error.
*
*    CONCATENATE gc_prefix iv_filepath INTO lv_doc_url.
*
*    CALL METHOD lo_documnt->open_document
*      EXPORTING
*        document_title = gc_excel
*        document_url   = lv_doc_url
*        open_inplace   = gc_x
*      IMPORTING
*        error          = lo_error.
*
*    IF lo_error->has_failed = gc_x.
*      CALL METHOD lo_error->raise_message
*        EXPORTING
*          type = gc_i.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*
*    CALL METHOD lo_documnt->get_spreadsheet_interface
*      EXPORTING
*        no_flush        = space
*      IMPORTING
*        error           = lo_error
*        sheet_interface = lo_spsheet.
*
*    IF lo_error->has_failed = gc_x.
*      CALL METHOD lo_error->raise_message
*        EXPORTING
*          type = gc_i.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*
*    CALL METHOD lo_spsheet->get_sheets
*      EXPORTING
*        no_flush = space
*      IMPORTING
*        sheets   = lt_sheets
*        error    = lo_error.
*
*    IF lo_error->has_failed = gc_x.
*      CALL METHOD lo_error->raise_message
*        EXPORTING
*          type = gc_i.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*
*    LOOP AT lt_sheets INTO DATA(ls_sheets).
*
*      CALL METHOD lo_spsheet->select_sheet
*        EXPORTING
*          name  = ls_sheets-sheet_name
*        IMPORTING
*          error = lo_error.
*
*      IF lo_error->has_failed = gc_x.
*        EXIT.
*      ENDIF.
*
*      CALL METHOD lo_spsheet->set_selection
*        EXPORTING
*          top     = 1
*          left    = 1
*          rows    = iv_rows
*          columns = iv_cols.
*
*      CALL METHOD lo_spsheet->insert_range
*        EXPORTING
*          name     = gc_test
*          rows     = iv_rows
*          columns  = iv_cols
*          no_flush = space
*        IMPORTING
*          error    = lo_error.
*
*      IF lo_error->has_failed = gc_x.
*        EXIT.
*      ENDIF.
*
*      CLEAR: lt_data[].
*
*      CALL METHOD lo_spsheet->get_ranges_data
*        EXPORTING
*          all      = gc_x
*        IMPORTING
*          contents = lt_data
*          error    = lo_error
*        CHANGING
*          ranges   = lr_ranges.
*
**      DELETE lt_data WHERE value IS INITIAL OR value = space.
**
**      LOOP AT lt_data INTO DATA(ls_data).
**
**        APPEND VALUE #( sheet  = ls_sheets-sheet_name
**                        row    = ls_data-row
**                        col    = ls_data-column
**                        value  = ls_data-value ) TO es_content-content.
**
**        READ TABLE es_content-meta ASSIGNING FIELD-SYMBOL(<fs_meta>) WITH KEY sheet = ls_sheets-sheet_name.
**
**        IF <fs_meta> IS NOT ASSIGNED.
**
**          APPEND VALUE #( sheet     = ls_sheets-sheet_name
**                          numcoluns = ls_data-column
**                          numlines  = ls_data-row ) TO es_content-meta.
**        ELSE.
**
**          IF <fs_meta>-numlines < ls_data-row.
**            <fs_meta>-numlines = ls_data-row.
**          ENDIF.
**
**          IF <fs_meta>-numcoluns < ls_data-column.
**            <fs_meta>-numcoluns = ls_data-column.
**          ENDIF.
**
**          UNASSIGN <fs_meta>.
**
**        ENDIF.
**
**      ENDLOOP.
*
*    ENDLOOP.
**
**    CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
**      EXPORTING
**        full_name     = iv_filepath
**      IMPORTING
**        stripped_name = es_content-filename.
**
**    CALL METHOD lo_documnt->close_document
**      IMPORTING
**        error = lo_error.
**
**    CALL METHOD lo_documnt->release_document
**      IMPORTING
**        error = lo_error.
*
*  ENDMETHOD.
*
*
*  METHOD get_filename .
*
*    DATA:
*      lv_long_filename  TYPE chkfile,
*
*      lv_pure_filename  TYPE sdba_actid,
*      lv_pure_extension TYPE sdba_funct.
*
*    IF ( iv_file IS NOT INITIAL ) .
*
*      lv_long_filename = iv_file .
*
*      CALL FUNCTION 'SPLIT_FILENAME'
*        EXPORTING
*          long_filename  = lv_long_filename
*        IMPORTING
*          pure_filename  = lv_pure_filename
*          pure_extension = lv_pure_extension.
*
*      rv_file = |{ lv_pure_filename }.{ lv_pure_extension }| .
*
*    ENDIF .
*
*  ENDMETHOD .


*  METHOD get_jobname .
*
*    CONSTANTS:
*      lc_jobname TYPE tbtcjob-jobname VALUE 'ZSD_MR21'.
*
*    CLEAR rv_value .
*
*    rv_value = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }-| &&
*               |{ sy-uzeit(2) }.{ sy-uzeit+2(2) }.{ sy-uzeit+4(2) }| .
*
*  ENDMETHOD.
*
*
*  METHOD get_log .
*
*    CLEAR rt_value .
*
*    IF ( lines( me->gt_messages ) GT 0 ) .
*      rt_value = me->gt_messages .
*    ENDIF .
*
*  ENDMETHOD .
*
*
*  METHOD init_log .
*
*    CLEAR me->gt_messages .
*
*  ENDMETHOD .
*
*
*  METHOD save_log .
*
**    IF ( lines( me->gt_messages ) GT 0 ) .
**
**      zclsd_mr21_create_mass=>gv_handle =
**        zclca_log=>create_log_handle( iv_object    = gc_object
**                                      iv_subobject = gc_subobject
**                                      iv_extnumber = text-006 " Transaction logs ZSD_MC_MR21
**                                      it_return    = zclsd_mr21_create_mass=>gt_messages ) .
**
**    ENDIF .
*
*
*  ENDMETHOD .
*
*
*  METHOD upload_file .
*
*    DATA:
*      lv_ftfront       TYPE eseftappl,
*      lv_ftappl        TYPE eseftappl,
*      lv_flg_overwrite TYPE abap_bool,
*
*      rcgfiletr        TYPE rcgfiletr,
*      flg_stay         TYPE boolean.
*
*    CLEAR rv_value .
*
*    IF ( iv_dir  IS NOT INITIAL ) AND
*       ( iv_file IS NOT INITIAL ) .
*
*      lv_ftfront        = iv_file .
*      lv_ftappl         = |{ iv_dir }/{ get_jobname( ) }| .
*      lv_flg_overwrite  = abap_on .
*
*      rcgfiletr-ftfront = lv_ftfront .
*      rcgfiletr-ftappl  = lv_ftappl .
*      rcgfiletr-iefow   = lv_flg_overwrite .
*      rcgfiletr-ftftype = 'BIN' .
*      sy-cprog          = 'RC1TCG3Z' .
*
*      PERFORM l_exec_file_upload
*           IN PROGRAM saplc13z
*        USING rcgfiletr-ftfront
*              rcgfiletr-ftappl
*              rcgfiletr-iefow
*              rcgfiletr-ftftype
*     CHANGING flg_stay.
*
*      IF ( flg_stay EQ abap_false ) .
*        rv_value = lv_ftappl .
*      ENDIF.
*
*      sy-cprog          = 'ZSD_MR21_CREATE_MASS_COCKPIT' .
*
*    ENDIF .
*
*  ENDMETHOD .

ENDCLASS.


SELECTION-SCREEN BEGIN OF BLOCK blc01 WITH FRAME TITLE text-001.
PARAMETERS:
  p_flpath TYPE string LOWER CASE OBLIGATORY,
  p_test   TYPE char1 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK blc01 .


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_flpath.
  p_flpath = zclfi_remove_cp_dup=>get_filepath( ).


INITIALIZATION.


START-OF-SELECTION.

  IF ( p_flpath IS NOT INITIAL ) . " Processamento em background

    DATA(lo_obj) = NEW zclfi_remove_cp_dup( ) .

    IF ( lo_obj IS BOUND ) .

      BREAK-POINT .

      lo_obj->fill_data_from_file( iv_pathfile = p_flpath ) .

      IF ( lo_obj->is_valid_data( ) EQ abap_true ) .

        lo_obj->process( iv_testrun = p_test ) .

      ELSE .

        lo_obj->log_save( ) .

      ENDIF .

      lo_obj->log_display( ) .

*      " Iniciar log de processamento
*      zclsd_mr21_create_mass=>init_log( ).
*      zclsd_mr21_create_mass=>shdb_fill_params( EXPORTING it_data   = gt_excel_data
*                                                IMPORTING et_params = DATA(gt_params) ) .
*      zclsd_mr21_create_mass=>shdb_execute( it_shdb_params = gt_params ).
*      zclsd_mr21_create_mass=>save_log( ).
*      zclsd_mr21_create_mass=>delete_file( p_fileap ) .

    ENDIF .

  ENDIF .

*  IF ( p_flpath IS NOT INITIAL ) . " Processamento em background
*
*    IF ( p_debug EQ abap_true ) .
*      DO . ENDDO .
*    ENDIF.
*
*    " Ler dados do arquivo no servidor de aplicacao
*    DATA(gt_excel_data) =
*      zclsd_mr21_create_mass=>fill_data_from_file( p_fileap ) .
*
*    " Iniciar log de processamento
*    zclsd_mr21_create_mass=>init_log( ).
*    zclsd_mr21_create_mass=>shdb_fill_params( EXPORTING it_data   = gt_excel_data
*                                              IMPORTING et_params = DATA(gt_params) ) .
*    zclsd_mr21_create_mass=>shdb_execute( it_shdb_params = gt_params ).
*    zclsd_mr21_create_mass=>save_log( ).
*    zclsd_mr21_create_mass=>delete_file( p_fileap ) .
*
*  ELSE .
*
*    " Fazer upload do arquivo no servidor
*
*    zclca_fixedvals=>get_cons_val( EXPORTING  iv_bukrs = space
*                                              iv_modul = zclca_fixedvals=>gc_module_sd
*                                              iv_proce = gc_proce
*                                              iv_fname = gc_fname
*                                              iv_seque = 1
*                                   IMPORTING  ev_const = gv_diretorio
*                                   EXCEPTIONS no_data  = 1
*                                              OTHERS   = 2 ) .
*    IF ( sy-subrc EQ 0 ) .
*
*      " Verificar se diretorio exist
*      IF ( zclsd_mr21_create_mass=>check_diretorio( gv_diretorio ) EQ abap_true ) .
*
*        DATA(lv_file) = zclsd_mr21_create_mass=>upload_file( iv_file = p_flpath
*                                                             iv_dir  = gv_diretorio ) .
*
*        " Verificar se arquivo existe
*        IF ( zclsd_mr21_create_mass=>check_arquivo( lv_file ) EQ abap_true ) .
*          zclsd_mr21_create_mass=>bg_execute_server( iv_file  = lv_file
*                                                     iv_debug = gv_debug ).
*          zclsd_mr21_create_mass=>add_log( VALUE #( type       = if_xo_const_message=>info
*                                                    id         = zclsd_mr21_create_mass=>gc_id
*                                                    number     = zclsd_mr21_create_mass=>gc_number
*                                                    message_v1 = text-008 ) ). " Exec. em background. Acesse SLG1 p/ log (obj.ZSD / sub.ZSD_MR21)
*          IF ( lines( zclsd_mr21_create_mass=>get_log( ) ) GT 0 ) .
*
*            CALL FUNCTION 'UDM_MESSAGE_SHOW'
*              EXPORTING
*                et_message = zclsd_mr21_create_mass=>get_log( ).
*
*            " Adicionar mensagem com o arquivo que sera processado
**            zclsd_mr21_create_mass=>init_log( ).
*            zclsd_mr21_create_mass=>add_log( VALUE #( type       = if_xo_const_message=>info
*                                                      id         = zclsd_mr21_create_mass=>gc_id
*                                                      number     = zclsd_mr21_create_mass=>gc_number
*                                                      message_v1 = text-009 ) ). " Arquivo sera processado em background.
*
*            zclsd_mr21_create_mass=>add_log( VALUE #( type       = if_xo_const_message=>info
*                                                      id         = zclsd_mr21_create_mass=>gc_id
*                                                      number     = zclsd_mr21_create_mass=>gc_number
*                                                      message_v1 = text-010 " Arquivo:
*                                                      message_v2 = lv_file ) ) .
*            zclsd_mr21_create_mass=>save_log( ).
*
*            CALL FUNCTION 'UDM_MESSAGE_SHOW'
*              EXPORTING
*                et_message = zclsd_mr21_create_mass=>get_log( ).
*
*            zclsd_mr21_create_mass=>init_log( ).
*
*          ENDIF .
*
*        ELSE .
*          " Não foi possível gerar o arquivo no servidor.
*          zclsd_mr21_create_mass=>add_log( VALUE #( type       = if_xo_const_message=>error
*                                                    id         = zclsd_mr21_create_mass=>gc_id
*                                                    number     = zclsd_mr21_create_mass=>gc_number
*                                                    message_v1 = text-011 ) ).
*
*        ENDIF .
*      ELSE .
*        " Não foi possivel acessar o diretorio.
*        zclsd_mr21_create_mass=>add_log( VALUE #( type       = if_xo_const_message=>error
*                                                  id         = zclsd_mr21_create_mass=>gc_id
*                                                  number     = zclsd_mr21_create_mass=>gc_number
*                                                  message_v1 = text-012 ) ).
*
*        zclsd_mr21_create_mass=>add_log( VALUE #( type       = if_xo_const_message=>error
*                                                  id         = zclsd_mr21_create_mass=>gc_id
*                                                  number     = zclsd_mr21_create_mass=>gc_number
*                                                  message_v1 = text-013 " Diretorio:
*                                                  message_v2 = gv_diretorio ) ) .
*
*      ENDIF .
*
*    ELSE .
*      " Nao foi encontrada a constante.
*      zclsd_mr21_create_mass=>add_log( VALUE #( type       = if_xo_const_message=>error
*                                                id         = zclsd_mr21_create_mass=>gc_id
*                                                number     = zclsd_mr21_create_mass=>gc_number
*                                                message_v1 = text-014 ) ).
*
*      zclsd_mr21_create_mass=>add_log( VALUE #( type       = if_xo_const_message=>error
*                                                id         = zclsd_mr21_create_mass=>gc_id
*                                                number     = zclsd_mr21_create_mass=>gc_number
*                                                message_v1 = text-015 " Constante:
*                                                message_v2 = |{ zclca_fixedvals=>gc_module_sd } · | &&
*                                                             |{ gc_proce } · { gc_fname }| ) ) .
*    ENDIF .
*
*    IF ( lines( zclsd_mr21_create_mass=>get_log( ) ) GT 0 ) .
*      zclsd_mr21_create_mass=>save_log( ).
*    ENDIF .
*
*
*  ENDIF .
