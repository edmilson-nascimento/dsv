*&---------------------------------------------------------------------*
*&  Include           ZOA_CONTACTPERSON_CLASSES
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_view DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:

      init_grid,

      check_for_screen_changes,

      refresh,

      change_display_mode IMPORTING iv_customer_number TYPE kunnr
                          RETURNING value(rv_mode)     TYPE string
                          RAISING   zcx_oa_contactperson,

      set_grid_in_display_mode,

      free,

      grid_is_initial RETURNING value(rv_grid_is_initial) TYPE abap_bool,

      protocol_message_exist RETURNING value(rv_result) TYPE int4.

  PRIVATE SECTION.
    DATA: mr_alv_grid       TYPE REF TO cl_gui_alv_grid,
          mr_event_receiver TYPE REF TO lcl_event_handler.

    METHODS:

      set_layout RETURNING value(rs_layout) TYPE lvc_s_layo,

      build_field_catalog RETURNING value(rt_field_catalog) TYPE lvc_t_fcat,

      create_dropdown RETURNING value(rt_dropdown) TYPE lvc_t_dral,

      hide_unused_toolbar_buttons RETURNING value(rt_excluded_functions) TYPE ui_functions.

ENDCLASS.                    "lcl_view DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_data DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_data DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      lock_customer IMPORTING iv_customer_number TYPE kunnr
                    RAISING   zcx_oa_contactperson,

      unlock_customer IMPORTING iv_customer_number TYPE kunnr
                      RAISING   zcx_oa_contactperson,

      get_contacts IMPORTING iv_customer_number     TYPE kunnr
                   RETURNING value(rt_alv_contacts) TYPE zoa_contactperson_tt
                   RAISING   zcx_oa_contactperson,

      check_customer_exist IMPORTING iv_customer_number TYPE kunnr
                           RAISING   zcx_oa_contactperson,

      save_contacts IMPORTING iv_customer_number TYPE kunnr
                              it_new_contacts    TYPE zoa_contactperson_tt
                    RAISING   zcx_oa_contactperson,

      save_contacts_single IMPORTING iv_customer_number TYPE kunnr
                                     it_new_contacts    TYPE zoa_contactperson_tt
                           RAISING   zcx_oa_contactperson,

      read_customer_name IMPORTING iv_customer_number      TYPE kunnr
                         RETURNING value(rv_customer_name) TYPE name1,

      data_is_valid RAISING zcx_oa_contactperson.


    DATA: mt_contacts TYPE STANDARD TABLE OF zoa_contactperson_st.

  PRIVATE SECTION.
    METHODS:

      raise_error IMPORTING is_error TYPE cvis_message
                  RAISING   zcx_oa_contactperson.

ENDCLASS.                    "lcl_data DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS handle_data_changed
                  FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.

    DATA mr_data_changed_prot TYPE REF TO cl_alv_changed_data_protocol.

  PRIVATE SECTION.

    METHODS check_mandatory_fields IMPORTING ir_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.                    "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_view IMPLEMENTATION.
  METHOD init_grid.

    IF mr_alv_grid IS BOUND.
      RETURN.
    ENDIF.

    DATA lr_container TYPE REF TO cl_gui_custom_container.

    CREATE OBJECT lr_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    CREATE OBJECT mr_alv_grid
      EXPORTING
        i_parent = lr_container.

    DATA lt_field_catalog TYPE lvc_t_fcat.
    lt_field_catalog = build_field_catalog( ).


    DATA ls_layout TYPE lvc_s_layo.
    ls_layout = set_layout( ).

    DATA lt_dropdown TYPE lvc_t_dral.
    lt_dropdown = create_dropdown( ).

    mr_alv_grid->set_drop_down_table( it_drop_down_alias = lt_dropdown ).

    " Hide unused buttons
    DATA lt_excluded_functions TYPE ui_functions.
    lt_excluded_functions = hide_unused_toolbar_buttons( ).

    DATA ls_variant TYPE disvariant.
    ls_variant-report = sy-repid.

    mr_alv_grid->register_edit_event( EXPORTING  i_event_id = cl_gui_alv_grid=>mc_evt_enter
                                      EXCEPTIONS error      = 1
                                                 OTHERS     = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " Create event handler
    CREATE OBJECT mr_event_receiver.
    SET HANDLER mr_event_receiver->handle_data_changed FOR mr_alv_grid.

    CALL METHOD mr_alv_grid->set_table_for_first_display
      EXPORTING
        i_save                        = 'A'
        i_default                     = abap_true
        is_layout                     = ls_layout
        is_variant                    = ls_variant
        it_toolbar_excluding          = lt_excluded_functions
      CHANGING
        it_outtab                     = gt_alv_contacts
        it_fieldcatalog               = lt_field_catalog
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " Initially open in Display mode
    mr_alv_grid->set_ready_for_input( i_ready_for_input = 0 ).

  ENDMETHOD.                    "init_grid
  METHOD set_layout.

    CLEAR rs_layout.

    rs_layout-zebra = abap_true.

  ENDMETHOD.                    "SET_LAYOUT
  METHOD build_field_catalog.

    CLEAR rt_field_catalog.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = gc_alv_structure_name
        i_client_never_display = abap_true
      CHANGING
        ct_fieldcat            = rt_field_catalog
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    FIELD-SYMBOLS <ls_field_catalog> LIKE LINE OF rt_field_catalog.

    LOOP AT rt_field_catalog ASSIGNING <ls_field_catalog>.
      CASE <ls_field_catalog>-fieldname.
        WHEN 'KUNNR' OR 'PARNR' OR 'PRSNR' OR 'ADRND'. " These fields should not be displayed (service data)
          <ls_field_catalog>-no_out     = abap_true.
          <ls_field_catalog>-edit       = abap_false.
        WHEN 'PAFKT'.
          <ls_field_catalog>-outputlen  = 18.
          <ls_field_catalog>-drdn_hndl  = '1'.
          <ls_field_catalog>-drdn_alias = abap_true.
          <ls_field_catalog>-edit       = abap_true.
        WHEN 'NAME_FIRST' OR 'NAME_LAST'.
          <ls_field_catalog>-outputlen = 20.
          <ls_field_catalog>-edit      = abap_true.
        WHEN 'COUNTRY'.
          <ls_field_catalog>-outputlen = 6.
          <ls_field_catalog>-edit      = abap_true.
        WHEN 'SMTP_ADDR'.
          <ls_field_catalog>-outputlen = 30.
          <ls_field_catalog>-edit      = abap_true.
        WHEN 'TEL_NUMBER' OR 'FAX_NUMBER'.
          <ls_field_catalog>-outputlen = 18.
          <ls_field_catalog>-edit      = abap_true.
        WHEN 'TEL_EXTENS' OR 'FAX_EXTENS'.
          <ls_field_catalog>-outputlen = 8.
          <ls_field_catalog>-edit      = abap_true.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.                    "build_field_catalog                   "data_is_valid
  METHOD check_for_screen_changes.

    IF mr_alv_grid IS BOUND.
      mr_alv_grid->check_changed_data( ).
    ENDIF.

  ENDMETHOD.                    "check_for_screen_changes
  METHOD refresh.

    IF mr_alv_grid IS BOUND.

      mr_alv_grid->refresh_table_display( EXPORTING i_soft_refresh = abap_true
                                          EXCEPTIONS finished = 1
                                                     OTHERS   = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.
  ENDMETHOD.                    "refresh
  METHOD create_dropdown.
    CLEAR rt_dropdown.

    DATA lt_tpfk TYPE STANDARD TABLE OF tpfkt.

    SELECT *
      FROM tpfkt
      INTO TABLE lt_tpfk
      WHERE spras = 'E'
      AND pafkt IN ('00', 'CI').

    DATA ls_dropdown TYPE lvc_s_dral.
    FIELD-SYMBOLS: <ls_tpfk> LIKE LINE OF lt_tpfk.

    ls_dropdown-handle = '1'.

    LOOP AT lt_tpfk ASSIGNING <ls_tpfk>.
      CONCATENATE <ls_tpfk>-pafkt <ls_tpfk>-vtext INTO ls_dropdown-value SEPARATED BY space.
*      ls_dropdown-value = <ls_tpfk>-vtext.
      ls_dropdown-int_value = <ls_tpfk>-pafkt.
      APPEND ls_dropdown TO rt_dropdown.
    ENDLOOP.

  ENDMETHOD.                    "create_dropdown
  METHOD change_display_mode.

    check_for_screen_changes( ).

    IF mr_alv_grid->is_ready_for_input( ) = 0.

      gr_data->lock_customer( iv_customer_number ).

      " Add empty row if enter in change mode and customer doesn't have any contacts
      IF gt_alv_contacts IS INITIAL.
        APPEND INITIAL LINE TO gt_alv_contacts.
        refresh( ).
      ENDIF.

      mr_alv_grid->set_ready_for_input( i_ready_for_input = 1 ).
      rv_mode = gc_change_mode.

    ELSE.
      IF gv_data_changed = abap_true.
        MESSAGE i023(zoa) DISPLAY LIKE 'W'.
*   Changed data is not saved yet

      ENDIF.
      gr_data->unlock_customer( iv_customer_number ).

      mr_alv_grid->set_ready_for_input( i_ready_for_input = 0 ).
      rv_mode = gc_display_mode.

    ENDIF.

  ENDMETHOD.                    "change_display_mode
  METHOD protocol_message_exist.
    IF mr_event_receiver->mr_data_changed_prot IS BOUND.
      mr_event_receiver->mr_data_changed_prot->protocol_is_visible( IMPORTING visible = rv_result ).
    ENDIF.
  ENDMETHOD.                    "protocol_message_exist
  METHOD set_grid_in_display_mode.

    IF mr_alv_grid IS BOUND.
      mr_alv_grid->set_ready_for_input( i_ready_for_input = 0 ).
    ENDIF.

  ENDMETHOD.                    "set_grid_in_display_mode
  METHOD hide_unused_toolbar_buttons.

    CLEAR rt_excluded_functions.

    DATA lv_excluded_function TYPE ui_func.

    lv_excluded_function = cl_gui_alv_grid=>mc_fc_subtot.
    APPEND lv_excluded_function TO rt_excluded_functions.

    lv_excluded_function = cl_gui_alv_grid=>mc_mb_sum.
    APPEND lv_excluded_function TO rt_excluded_functions.

    lv_excluded_function = cl_gui_alv_grid=>mc_fc_graph.
    APPEND lv_excluded_function TO rt_excluded_functions.

    lv_excluded_function = cl_gui_alv_grid=>mc_fc_info.
    APPEND lv_excluded_function TO rt_excluded_functions.

  ENDMETHOD.                    "hide_unused_toolbar_buttons
  METHOD free.
    IF mr_alv_grid IS BOUND.
      mr_alv_grid->free( ).
    ENDIF.
  ENDMETHOD.                    "free
  METHOD grid_is_initial.
    IF mr_alv_grid IS BOUND.
      rv_grid_is_initial = abap_false.
    ELSE.
      rv_grid_is_initial = abap_true.
    ENDIF.
  ENDMETHOD.                    "grid_is_initial
ENDCLASS.                    "lcl_view IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_data IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_data IMPLEMENTATION.
  METHOD lock_customer.
    DATA ls_error TYPE cvis_message.

    CALL METHOD cmd_ei_api=>lock
      EXPORTING
        iv_kunnr = iv_customer_number
      IMPORTING
        es_error = ls_error.

    IF ls_error-is_error = abap_true.
      raise_error( ls_error ).
    ENDIF.

  ENDMETHOD.                    "lock_customer
  METHOD unlock_customer.

    DATA lv_locked TYPE abap_bool.

    CALL METHOD cmd_ei_api=>get_lock
      EXPORTING
        iv_kunnr         = iv_customer_number
      IMPORTING
        ev_in_lock_table = lv_locked.

    IF lv_locked = abap_false.
      RETURN.
    ENDIF.


    DATA: ls_error TYPE cvis_message.

    CALL METHOD cmd_ei_api=>unlock
      EXPORTING
        iv_kunnr = iv_customer_number
      IMPORTING
        es_error = ls_error.

    IF ls_error-is_error EQ abap_true.
      raise_error( ls_error ).
    ENDIF.

  ENDMETHOD.                    "unlock_customer
  METHOD get_contacts.

    IF iv_customer_number IS INITIAL.
      RETURN.
    ENDIF.

    DATA: ls_contact  LIKE LINE OF mt_contacts,
          ls_return   TYPE bapireturn1,
          lv_messsage TYPE string,

          "lv_addrno   TYPE ad_addrnum,
          "lv_persno   TYPE ad_persnum,
          lv_adrnd    TYPE adrnd,
          lv_objkey_c TYPE bapi4003_1-objkey_c,
          lv_objkey_p TYPE bapi4003_1-objkey_p,

          lt_ad3vl    TYPE TABLE OF bapiad3vl,
          lt_adfax    TYPE TABLE OF bapiadfax,
          lt_adsmtp   TYPE TABLE OF bapiadsmtp,
          lt_adtel    TYPE TABLE OF bapiadtel,
          lt_return   TYPE bapiret2_t.

    FIELD-SYMBOLS:
      <ls_ad3vl>  TYPE bapiad3vl,
      <ls_addr1>  TYPE szadr_addr1_line,
      <ls_adfax>  TYPE bapiadfax,
      <ls_adsmtp> TYPE bapiadsmtp,
      <ls_adtel>  TYPE bapiadtel.
    "<ls_aduse>  TYPE bapiaduse,
*          <ls_return> TYPE bapiret2.

    CONSTANTS:
      co_obj_type_p TYPE bapi4003_1-objtype_p VALUE 'BUS1006001',
      co_obj_type_c TYPE bapi4003_1-objtype_c VALUE 'KNA1',
      co_context    TYPE bapi4003_1-context   VALUE '0005'.

    CLEAR mt_contacts.

    DATA lt_knvk TYPE STANDARD TABLE OF knvk.

    SELECT *
      INTO TABLE lt_knvk
      FROM knvk
      WHERE kunnr = iv_customer_number
      AND ( pafkt = '00' OR pafkt = 'CI' ). " For DSV Healthcare only functions: 00 (InvoiceE-mailReceiv) and CI Clerks internet are allowed

    FIELD-SYMBOLS <ls_knvk> LIKE LINE OF lt_knvk.

    DATA ls_addr_complete TYPE szadr_addr1_complete.

    LOOP AT lt_knvk ASSIGNING <ls_knvk>.

      CLEAR: ls_addr_complete,
             ls_contact,
             ls_return.

      ls_contact-kunnr = iv_customer_number.
      ls_contact-parnr = <ls_knvk>-parnr.
      ls_contact-pafkt = <ls_knvk>-pafkt.
      ls_contact-prsnr = <ls_knvk>-prsnr.
      ls_contact-adrnd = <ls_knvk>-adrnd.

      IF <ls_knvk>-prsnr IS INITIAL.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'ADDR_MEMORY_CLEAR'.

      lv_objkey_p = <ls_knvk>-parnr.
      lv_objkey_c = <ls_knvk>-kunnr.

      CALL FUNCTION 'BAPI_ADDRESSCONTPART_GETDETAIL'
        EXPORTING
          obj_type_p = co_obj_type_p
          obj_id_p   = lv_objkey_p
          obj_type_c = co_obj_type_c
          obj_id_c   = lv_objkey_c
          context    = co_context
          "IMPORTING
          "address_number = lv_addrno
          "person_number  = lv_persno
        TABLES
          bapiad3vl  = lt_ad3vl
          bapiadtel  = lt_adtel
          bapiadfax  = lt_adfax
          bapiadsmtp = lt_adsmtp
          return     = lt_return.

      LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
        MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
        WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
        INTO lv_messsage.

        RAISE EXCEPTION TYPE zcx_oa_contactperson
          EXPORTING
            text = lv_messsage.
      ENDLOOP.


      " Name data
      LOOP AT lt_ad3vl ASSIGNING <ls_ad3vl>
        WHERE addr_vers IS INITIAL AND
              from_date <= sy-datum AND
              to_date   >= sy-datum.

        ls_contact-name_first = <ls_ad3vl>-firstname.
        ls_contact-name_last  = <ls_ad3vl>-lastname.
      ENDLOOP.

      " Telephone number
      LOOP AT lt_adtel ASSIGNING <ls_adtel>
        WHERE std_no = abap_true AND
              errorflag IS INITIAL AND
              flg_nouse IS INITIAL.

        ls_contact-tel_number = <ls_adtel>-telephone.
        ls_contact-tel_extens = <ls_adtel>-extension.
      ENDLOOP.

      " Fax number
      LOOP AT lt_adfax ASSIGNING <ls_adfax>
        WHERE std_no = abap_true AND
              errorflag IS INITIAL AND
              flg_nouse IS INITIAL.

        ls_contact-fax_number = <ls_adfax>-fax.
        ls_contact-fax_extens = <ls_adfax>-extension.
      ENDLOOP.

      " E-mail
      LOOP AT lt_adsmtp ASSIGNING <ls_adsmtp>
        WHERE std_no = abap_true AND
              errorflag IS INITIAL AND
              flg_nouse IS INITIAL.

        ls_contact-smtp_addr  = <ls_adsmtp>-e_mail.
      ENDLOOP.

      IF <ls_knvk>-adrnd IS NOT INITIAL. " Business address exists
        " => Get country code

        CALL FUNCTION 'ADDR_GET_COMPLETE'
          EXPORTING
            addrnumber              = <ls_knvk>-adrnd
          IMPORTING
            addr1_complete          = ls_addr_complete
          EXCEPTIONS
            parameter_error         = 1
            address_not_exist       = 2
            internal_error          = 3
            wrong_access_to_archive = 4
            address_blocked         = 5
            OTHERS                  = 6.

        IF sy-subrc <> 0.
          CASE sy-subrc.
            WHEN 1.
              MESSAGE ID 'AM' TYPE 'E' NUMBER '001'
                WITH 'Parameter error when calling'
                     'ADDR_GET_COMPLETE'
                INTO lv_messsage.
            WHEN 2.
              MESSAGE ID 'AM' TYPE 'E' NUMBER '010'
                WITH lv_adrnd
                INTO lv_messsage.
            WHEN 3.
              MESSAGE ID 'AM' TYPE 'E' NUMBER '001'
                WITH 'Internal error when calling'
                     'ADDR_GET_COMPLETE'
                INTO lv_messsage.
            WHEN 4.
              MESSAGE ID 'AM' TYPE 'E' NUMBER '001'
                WITH 'Wrong access to archive'
                     'when calling ADDR_GET_COMPLETE'
                INTO lv_messsage.
            WHEN 5.
              MESSAGE ID 'AM' TYPE 'E' NUMBER '774'
              INTO lv_messsage.
            WHEN 6.
              MESSAGE ID 'AM' TYPE 'E' NUMBER '001'
                WITH 'Unknown error'
                     'when calling ADDR_GET_COMPLETE'
              INTO lv_messsage.
          ENDCASE.

          RAISE EXCEPTION TYPE zcx_oa_contactperson
            EXPORTING
              text = lv_messsage.
        ENDIF.

        LOOP AT ls_addr_complete-addr1_tab ASSIGNING <ls_addr1>.
          ls_contact-country = <ls_addr1>-data-country.
        ENDLOOP.

      ENDIF.

      APPEND ls_contact TO mt_contacts.

    ENDLOOP.

    rt_alv_contacts = mt_contacts.

  ENDMETHOD.                    "get_contacts
  METHOD check_customer_exist.

    DATA: ls_return  TYPE bapiret1,
          lv_message TYPE string.

    CALL FUNCTION 'BAPI_CUSTOMER_EXISTENCECHECK'
      EXPORTING
        customerno  = iv_customer_number
        companycode = gc_dsv_healthcare_cc
      IMPORTING
        return      = ls_return.

    IF ls_return-type CA 'AE'.
      MESSAGE ID ls_return-id
        TYPE ls_return-type
        NUMBER ls_return-number
        WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
        INTO lv_message.

      RAISE EXCEPTION TYPE zcx_oa_contactperson
        EXPORTING
          text = lv_message.
    ENDIF.

  ENDMETHOD.                    "check_customer_exist
  METHOD read_customer_name.

    CLEAR rv_customer_name.

    SELECT SINGLE name1
      FROM kna1
      INTO rv_customer_name
      WHERE kunnr = iv_customer_number.

  ENDMETHOD.                    "read_customer_name
  METHOD save_contacts.

    DATA: lt_customers   TYPE cmds_ei_extern_t,
          ls_customer    LIKE LINE OF lt_customers,

          ls_master_data TYPE cmds_ei_main,
          ls_contact     TYPE cmds_ei_contacts,
          ls_smtp        TYPE cvis_ei_smtp_str,
          ls_phone       TYPE cvis_ei_phone_str,
          ls_fax         TYPE cvis_ei_fax_str,

          ls_error       TYPE cvis_message.

    FIELD-SYMBOLS: <ls_new_contact> LIKE LINE OF mt_contacts,
                   <ls_old_contact> LIKE LINE OF mt_contacts.

    " Read the original contacts if needed
    IF mt_contacts IS INITIAL.
      get_contacts( iv_customer_number ).
    ENDIF.


    LOOP AT it_new_contacts ASSIGNING <ls_new_contact>.

      CLEAR: ls_contact,
             ls_smtp.

      IF <ls_new_contact>-parnr IS INITIAL.

        ls_contact-task = 'I'.
        " Generate new contact number
        cmd_ei_api=>get_contact_number( IMPORTING ev_parnr = ls_contact-data_key-parnr
                                                  es_error = ls_error ).
        IF ls_error-is_error = abap_true.
          raise_error( ls_error ).
        ENDIF.

      ELSE.
        ls_contact-task = 'U'.
        ls_contact-data_key-parnr = <ls_new_contact>-parnr.
      ENDIF.

      " Partner Function
      ls_contact-data-pafkt  = <ls_new_contact>-pafkt.
      ls_contact-datax-pafkt = abap_true.

      " Person data - name
      IF <ls_new_contact>-prsnr IS INITIAL.
        ls_contact-address_type_3-task = 'I'.
      ELSE.
        ls_contact-address_type_3-task = 'U'.
      ENDIF.

      ls_contact-address_type_3-postal-data-firstname = <ls_new_contact>-name_first.
      ls_contact-address_type_3-postal-data-lastname  = <ls_new_contact>-name_last.

      ls_contact-address_type_3-postal-datax-firstname = abap_true.
      ls_contact-address_type_3-postal-datax-lastname  = abap_true.

      " Business Address data - country
      IF <ls_new_contact>-adrnd IS INITIAL.
        ls_contact-address_type_1-task = 'I'.
      ELSE.
        ls_contact-address_type_1-task = 'U'.
      ENDIF.

      ls_contact-address_type_1-postal-data-country  = <ls_new_contact>-country.
      CONCATENATE <ls_new_contact>-name_first <ls_new_contact>-name_last INTO ls_contact-address_type_1-postal-data-name SEPARATED BY space.
      ls_contact-address_type_1-postal-datax-country = abap_true.
      ls_contact-address_type_1-postal-datax-name = abap_true.


*      UNASSIGN <ls_old_contact>.

      READ TABLE mt_contacts ASSIGNING <ls_old_contact>
        WITH KEY parnr = <ls_new_contact>-parnr.

      if ( <ls_old_contact> is assigned ) .

        " E-mail address
        IF <ls_new_contact>-smtp_addr IS INITIAL. " E-mail is deleted
          ls_smtp-contact-task = 'D'.
        ELSEIF <ls_old_contact> IS ASSIGNED AND <ls_old_contact>-smtp_addr IS NOT INITIAL.
          ls_smtp-contact-task = 'U'.
        ELSE.
          ls_smtp-contact-task = 'I'.
        ENDIF.

        ls_smtp-contact-data-e_mail = <ls_new_contact>-smtp_addr.
        ls_smtp-contact-datax-e_mail = abap_true.
        APPEND ls_smtp TO ls_contact-address_type_3-communication-smtp-smtp.

        " Telephone number
        IF <ls_new_contact>-tel_number IS INITIAL. " Telephone is deleted
          ls_phone-contact-task = 'D'.
        ELSEIF <ls_old_contact> IS ASSIGNED AND <ls_old_contact>-tel_number IS NOT INITIAL.
          ls_phone-contact-task = 'U'.
        ELSE.
          ls_phone-contact-task = 'I'.
        ENDIF.

        ls_phone-contact-data-telephone  = <ls_new_contact>-tel_number.
        ls_phone-contact-data-extension  = <ls_new_contact>-tel_extens.
        ls_phone-contact-data-country    = <ls_new_contact>-country. " Telephone country code

        ls_phone-contact-datax-telephone = abap_true.
        ls_phone-contact-datax-extension = abap_true.
        ls_phone-contact-datax-country   = abap_true.
        APPEND ls_phone TO ls_contact-address_type_3-communication-phone-phone.


        " Fax number
        IF <ls_new_contact>-fax_number IS INITIAL. " Fax number is deleted
          ls_fax-contact-task = 'D'.
        ELSEIF <ls_old_contact> IS ASSIGNED AND <ls_old_contact>-fax_number IS NOT INITIAL.
          ls_fax-contact-task = 'U'.
        ELSE.
          ls_fax-contact-task = 'I'.
        ENDIF.

        ls_fax-contact-data-fax        = <ls_new_contact>-fax_number.
        ls_fax-contact-data-extension  = <ls_new_contact>-fax_extens.
        ls_fax-contact-data-country    = <ls_new_contact>-country. " Fax country code

        ls_fax-contact-datax-fax       = abap_true.
        ls_fax-contact-datax-extension = abap_true.
        ls_fax-contact-datax-country   = abap_true.
        APPEND ls_fax TO ls_contact-address_type_3-communication-fax-fax.

        unassign <ls_old_contact> .
      endif .

      APPEND ls_contact TO ls_customer-central_data-contact-contacts.

    ENDLOOP.

    " Delete removed entries
    LOOP AT mt_contacts ASSIGNING <ls_new_contact>.
      CLEAR ls_contact.

      READ TABLE it_new_contacts WITH KEY parnr = <ls_new_contact>-parnr
        TRANSPORTING NO FIELDS.

      CHECK sy-subrc <> 0.

      ls_contact-task = 'D'.
      ls_contact-data_key-parnr = <ls_new_contact>-parnr.

      APPEND ls_contact TO ls_customer-central_data-contact-contacts.

    ENDLOOP.

    ls_customer-header-object_instance-kunnr = iv_customer_number.
    ls_customer-header-object_task = 'C'." 'M'
    APPEND ls_customer TO ls_master_data-customers.

    " Persist data
    cmd_ei_api=>maintain( EXPORTING is_master_data = ls_master_data
                          IMPORTING es_error       = ls_error ).

    IF ls_error-is_error = abap_true.
      raise_error( ls_error ).
    ENDIF.

    cmd_ei_api=>update_modules( IMPORTING es_error = ls_error ).

    IF ls_error-is_error = abap_true.
      raise_error( ls_error ).
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

  ENDMETHOD.                    "save_contacts

  method save_contacts_single .

    data:
      lt_customers   type cmds_ei_extern_t,
      ls_customer    type cmds_ei_extern,

      ls_master_data type cmds_ei_main,
      ls_contact     type cmds_ei_contacts,
      ls_smtp        type cvis_ei_smtp_str,
      ls_phone       type cvis_ei_phone_str,
      ls_fax         type cvis_ei_fax_str,

      ls_error       type cvis_message.

    " Read the original contacts if needed
    if mt_contacts is initial.
      get_contacts( iv_customer_number ).
    endif.

**********************************************************************
    " Verificando INSERT
    loop at it_new_contacts assigning field-symbol(<ls_new_contact>)
      where parnr is initial .

      clear:
      ls_contact, ls_smtp.

      ls_contact-task = 'I'.
      " Generate new contact number
      cmd_ei_api=>get_contact_number( importing ev_parnr = ls_contact-data_key-parnr
        es_error = ls_error ).
      if ls_error-is_error = abap_true.
        raise_error( ls_error ).
      endif.


      " Partner Function
      ls_contact-data-pafkt  = <ls_new_contact>-pafkt.
      ls_contact-datax-pafkt = abap_true.

      " Person data - name
      if <ls_new_contact>-prsnr is initial.
        ls_contact-address_type_3-task = 'I'.
      else.
        ls_contact-address_type_3-task = 'U'.
      endif.

      ls_contact-address_type_3-postal-data-firstname = <ls_new_contact>-name_first.
      ls_contact-address_type_3-postal-data-lastname  = <ls_new_contact>-name_last.

      ls_contact-address_type_3-postal-datax-firstname = abap_true.
      ls_contact-address_type_3-postal-datax-lastname  = abap_true.

      " Business Address data - country
      if <ls_new_contact>-adrnd is initial.
        ls_contact-address_type_1-task = 'I'.
      else.
        ls_contact-address_type_1-task = 'U'.
      endif.

      ls_contact-address_type_1-postal-data-country  = <ls_new_contact>-country.
      concatenate <ls_new_contact>-name_first <ls_new_contact>-name_last
      into ls_contact-address_type_1-postal-data-name
            separated by space.
      ls_contact-address_type_1-postal-datax-country = abap_true.
      ls_contact-address_type_1-postal-datax-name = abap_true.


*      UNASSIGN <ls_old_contact>.

      read table mt_contacts assigning field-symbol(<ls_old_contact>)
        with key parnr = <ls_new_contact>-parnr.

      " E-mail address
      if <ls_new_contact>-smtp_addr is initial. " E-mail is deleted
        ls_smtp-contact-task = 'D'.
      elseif <ls_old_contact> is assigned and <ls_old_contact>-smtp_addr is not initial.
        ls_smtp-contact-task = 'U'.
      else.
        ls_smtp-contact-task = 'I'.
      endif.

      ls_smtp-contact-data-e_mail = <ls_new_contact>-smtp_addr.
      ls_smtp-contact-datax-e_mail = abap_true.
      append ls_smtp to ls_contact-address_type_3-communication-smtp-smtp.

      " Telephone number
      if <ls_new_contact>-tel_number is initial. " Telephone is deleted
        ls_phone-contact-task = 'D'.
      elseif <ls_old_contact> is assigned and <ls_old_contact>-tel_number is not initial.
        ls_phone-contact-task = 'U'.
      else.
        ls_phone-contact-task = 'I'.
      endif.

      ls_phone-contact-data-telephone  = <ls_new_contact>-tel_number.
      ls_phone-contact-data-extension  = <ls_new_contact>-tel_extens.
      ls_phone-contact-data-country    = <ls_new_contact>-country. " Telephone country code

      ls_phone-contact-datax-telephone = abap_true.
      ls_phone-contact-datax-extension = abap_true.
      ls_phone-contact-datax-country   = abap_true.
      append ls_phone to ls_contact-address_type_3-communication-phone-phone.


      " Fax number
      if <ls_new_contact>-fax_number is initial. " Fax number is deleted
        ls_fax-contact-task = 'D'.
      elseif <ls_old_contact> is assigned and <ls_old_contact>-fax_number is not initial.
        ls_fax-contact-task = 'U'.
      else.
        ls_fax-contact-task = 'I'.
      endif.

      ls_fax-contact-data-fax        = <ls_new_contact>-fax_number.
      ls_fax-contact-data-extension  = <ls_new_contact>-fax_extens.
      ls_fax-contact-data-country    = <ls_new_contact>-country. " Fax country code

      ls_fax-contact-datax-fax       = abap_true.
      ls_fax-contact-datax-extension = abap_true.
      ls_fax-contact-datax-country   = abap_true.
      append ls_fax to ls_contact-address_type_3-communication-fax-fax.

      unassign <ls_old_contact> .

      append ls_contact to ls_customer-central_data-contact-contacts.

    endloop .

    if ( lines( ls_customer-central_data-contact-contacts ) gt 0 ) .

      ls_customer-header-object_instance-kunnr = iv_customer_number.
      ls_customer-header-object_task = 'C'." 'M'
      append ls_customer to ls_master_data-customers.
      clear  ls_customer .

      " Persist data
      cmd_ei_api=>maintain( exporting is_master_data = ls_master_data
      importing es_error       = ls_error ).
      if ls_error-is_error = abap_true.
        raise_error( ls_error ).
      endif.

      cmd_ei_api=>update_modules( importing es_error = ls_error ).
      if ls_error-is_error = abap_true.
        raise_error( ls_error ).
      endif.

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.

    endif .

    clear ls_master_data .


**********************************************************************
    " Verificando DELETE
    loop at mt_contacts assigning <ls_new_contact>.
      clear ls_contact.

      read table it_new_contacts with key parnr = <ls_new_contact>-parnr
      transporting no fields.

      check sy-subrc <> 0.

      ls_contact-task = 'D'.
      ls_contact-data_key-parnr = <ls_new_contact>-parnr.

      append ls_contact to ls_customer-central_data-contact-contacts.

    endloop.

    if ( lines( ls_customer-central_data-contact-contacts ) gt 0 ) .

      ls_customer-header-object_instance-kunnr = iv_customer_number.
      ls_customer-header-object_task = 'C'." 'M'
      append ls_customer to ls_master_data-customers.
      clear  ls_customer .

      " Persist data
      cmd_ei_api=>maintain( exporting is_master_data = ls_master_data
      importing es_error       = ls_error ).
      if ls_error-is_error = abap_true.
        raise_error( ls_error ).
      endif.

      cmd_ei_api=>update_modules( importing es_error = ls_error ).
      if ls_error-is_error = abap_true.
        raise_error( ls_error ).
      endif.

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.

      clear ls_master_data .

    endif .



**********************************************************************
    " Verificando UPDATE

    loop at it_new_contacts assigning <ls_new_contact>
      where parnr is not initial .

      " Verificando itens que foram alterados
      assign mt_contacts[ kunnr = <ls_new_contact>-kunnr
                          parnr = <ls_new_contact>-parnr ]
      to field-symbol(<fs_contacts>) .
      if ( sy-subrc eq 0 ) .
        if ( <ls_new_contact> eq <fs_contacts> ) .
          continue .
        endif .
        unassign <fs_contacts> .
      endif .

      clear:
        ls_contact, ls_smtp.

      ls_contact-task = 'U'.
      ls_contact-data_key-parnr = <ls_new_contact>-parnr.

      " Partner Function
      ls_contact-data-pafkt  = <ls_new_contact>-pafkt.
      ls_contact-datax-pafkt = abap_true.

      " Person data - name
      if <ls_new_contact>-prsnr is initial.
        ls_contact-address_type_3-task = 'I'.
      else.
        ls_contact-address_type_3-task = 'U'.
      endif.

      ls_contact-address_type_3-postal-data-firstname = <ls_new_contact>-name_first.
      ls_contact-address_type_3-postal-data-lastname  = <ls_new_contact>-name_last.

      ls_contact-address_type_3-postal-datax-firstname = abap_true.
      ls_contact-address_type_3-postal-datax-lastname  = abap_true.

      " Business Address data - country
      if <ls_new_contact>-adrnd is initial.
        ls_contact-address_type_1-task = 'I'.
      else.
        ls_contact-address_type_1-task = 'U'.
      endif.

      ls_contact-address_type_1-postal-data-country  = <ls_new_contact>-country.
      concatenate <ls_new_contact>-name_first <ls_new_contact>-name_last into ls_contact-address_type_1-postal-data-name separated by space.
      ls_contact-address_type_1-postal-datax-country = abap_true.
      ls_contact-address_type_1-postal-datax-name = abap_true.


      read table mt_contacts assigning <ls_old_contact> with key parnr = <ls_new_contact>-parnr.

      " E-mail address
      if <ls_new_contact>-smtp_addr is initial. " E-mail is deleted
        ls_smtp-contact-task = 'D'.
      elseif <ls_old_contact> is assigned and <ls_old_contact>-smtp_addr is not initial.
        ls_smtp-contact-task = 'U'.
      else.
        ls_smtp-contact-task = 'I'.
      endif.

      ls_smtp-contact-data-e_mail = <ls_new_contact>-smtp_addr.
      ls_smtp-contact-datax-e_mail = abap_true.
      append ls_smtp to ls_contact-address_type_3-communication-smtp-smtp.

      " Telephone number
      if <ls_new_contact>-tel_number is initial. " Telephone is deleted
        ls_phone-contact-task = 'D'.
      elseif <ls_old_contact> is assigned and <ls_old_contact>-tel_number is not initial.
        ls_phone-contact-task = 'U'.
      else.
        ls_phone-contact-task = 'I'.
      endif.

      ls_phone-contact-data-telephone  = <ls_new_contact>-tel_number.
      ls_phone-contact-data-extension  = <ls_new_contact>-tel_extens.
      ls_phone-contact-data-country    = <ls_new_contact>-country. " Telephone country code

      ls_phone-contact-datax-telephone = abap_true.
      ls_phone-contact-datax-extension = abap_true.
      ls_phone-contact-datax-country   = abap_true.
      append ls_phone to ls_contact-address_type_3-communication-phone-phone.


      " Fax number
      if <ls_new_contact>-fax_number is initial. " Fax number is deleted
        ls_fax-contact-task = 'D'.
      elseif <ls_old_contact> is assigned and <ls_old_contact>-fax_number is not initial.
        ls_fax-contact-task = 'U'.
      else.
        ls_fax-contact-task = 'I'.
      endif.

      ls_fax-contact-data-fax        = <ls_new_contact>-fax_number.
      ls_fax-contact-data-extension  = <ls_new_contact>-fax_extens.
      ls_fax-contact-data-country    = <ls_new_contact>-country. " Fax country code

      ls_fax-contact-datax-fax       = abap_true.
      ls_fax-contact-datax-extension = abap_true.
      ls_fax-contact-datax-country   = abap_true.
      append ls_fax to ls_contact-address_type_3-communication-fax-fax.

      append ls_contact to ls_customer-central_data-contact-contacts.

    endloop.

    if ( lines( ls_customer-central_data-contact-contacts ) gt 0 ) .

      ls_customer-header-object_instance-kunnr = iv_customer_number.
      ls_customer-header-object_task = 'C'." 'M'
      append ls_customer to ls_master_data-customers.
      clear  ls_customer .

      " Persist data
      cmd_ei_api=>maintain( exporting is_master_data = ls_master_data
      importing es_error       = ls_error ).
      if ls_error-is_error = abap_true.
        raise_error( ls_error ).
      endif.

      cmd_ei_api=>update_modules( importing es_error = ls_error ).
      if ls_error-is_error = abap_true.
        raise_error( ls_error ).
      endif.

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.

      clear ls_master_data .

    endif .

  endmethod .


  METHOD raise_error.

    DATA lv_message TYPE string.

    FIELD-SYMBOLS <ls_error> TYPE bapiret2.

    LOOP AT is_error-messages ASSIGNING <ls_error> WHERE type CA 'AE'.

      MESSAGE ID <ls_error>-id TYPE <ls_error>-type NUMBER <ls_error>-number
        WITH <ls_error>-message_v1 <ls_error>-message_v2 <ls_error>-message_v3 <ls_error>-message_v4
        INTO lv_message.

      RAISE EXCEPTION TYPE zcx_oa_contactperson
        EXPORTING
          text = lv_message.

    ENDLOOP.
  ENDMETHOD.                    "raise_error
  METHOD data_is_valid.

    DATA lv_message TYPE string.

    FIELD-SYMBOLS <ls_alv_contact> LIKE LINE OF gt_alv_contacts.

    LOOP AT gt_alv_contacts ASSIGNING <ls_alv_contact> WHERE pafkt IS INITIAL OR
                                                             name_first IS INITIAL OR
                                                             name_last IS INITIAL OR
                                                             country IS INITIAL.
      MESSAGE w024(zoa) INTO lv_message.
*   Fill all mandatory fields: Function, First and Last Names, Country

      RAISE EXCEPTION TYPE zcx_oa_contactperson
        EXPORTING
          text = lv_message.

    ENDLOOP.
  ENDMETHOD.                    "data_is_valid
ENDCLASS.                    "lcl_data IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_data_changed.

    check_mandatory_fields( er_data_changed ).

    gv_data_changed = abap_true.

  ENDMETHOD.                    "handle_data_changed
  METHOD check_mandatory_fields.

    FIELD-SYMBOLS <ls_row> TYPE lvc_s_modi.

    LOOP AT ir_data_changed->mt_good_cells ASSIGNING <ls_row>.
      IF ( <ls_row>-fieldname = 'PAFKT'
        OR <ls_row>-fieldname = 'NAME_FIRST'
        OR <ls_row>-fieldname = 'NAME_LAST'
        OR <ls_row>-fieldname = 'COUNTRY' )
        AND <ls_row>-value IS INITIAL.

        ir_data_changed->add_protocol_entry( i_msgid     = 'ZOA'
                                             i_msgno     = '022' "Please fill mandatory field
                                             i_msgty     = 'E'
                                             i_fieldname = <ls_row>-fieldname
                                             i_row_id    = <ls_row>-row_id ).
      ENDIF.
    ENDLOOP.

    ir_data_changed->display_protocol( ).

    "Keep it as later we can check if errors exist
    mr_data_changed_prot = ir_data_changed.

  ENDMETHOD.                    "check_mandatory_fields
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION
