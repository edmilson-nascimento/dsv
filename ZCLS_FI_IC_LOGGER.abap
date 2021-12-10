class ZCLS_FI_IC_LOGGER definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_OBJECT type BALOBJ_D
      !IV_SUB_OBJECT type BALSUBOBJ .
  methods LOG_MESSAGE
    importing
      !IS_MESSAGE type BAPIRET2 optional .
  methods PERSIST_DATA .
  methods DISPLAY_DATA .
  methods READ_LOG_NUMBER
    returning
      value(RV_LOG_NUM) type BALOGNR .
protected section.
private section.

  data MV_UUID type SYSUUID_X16 .
  data MV_LOG_HANDLE type BALLOGHNDL .
  data MV_LOG_NUMBER type BALOGNR .
ENDCLASS.



CLASS ZCLS_FI_IC_LOGGER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLS_FI_IC_LOGGER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJECT                      TYPE        BALOBJ_D
* | [--->] IV_SUB_OBJECT                  TYPE        BALSUBOBJ
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    " Create UUID used for identifying group of messages
    TRY.
        mv_uuid = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
    ENDTRY.

    DATA(ls_log) = VALUE bal_s_log( extnumber = mv_uuid
                                    object    = iv_object
                                    subobject = iv_sub_object
                                    alprog    = sy-cprog
                                    aldate    = sy-datum
                                    altime    = sy-uzeit
                                    aluser    = sy-uname ).

    " Create application log handle
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = mv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      "Error handling
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLS_FI_IC_LOGGER->DISPLAY_DATA
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display_data.

    DATA(lt_handles) = VALUE bal_t_logh( ( mv_log_handle ) ).

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle       = lt_handles
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.

    IF sy-subrc <> 0.
      "Error handling
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLS_FI_IC_LOGGER->LOG_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MESSAGE                     TYPE        BAPIRET2(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD log_message.

    GET TIME.
    IF is_message IS INITIAL. " No explicit message data passed - use system structure

      DATA(ls_log) = VALUE bal_s_msg( msgty     = sy-msgty
                                      msgid     = sy-msgid
                                      msgno     = sy-msgno
                                      msgv1     = sy-msgv1
                                      msgv2     = sy-msgv2
                                      msgv3     = sy-msgv3
                                      msgv4     = sy-msgv4
                                      time_stmp = CONV #( sy-datum && sy-uzeit )
                                      probclass = COND #( WHEN sy-msgty = 'E' THEN '1'  "Very important
                                                          WHEN sy-msgty = 'W' THEN '2'  "Important
                                                          ELSE '3' ) ).                 "Medium

    ELSE.

      ls_log = VALUE bal_s_msg( msgty     = is_message-type
                                msgid     = is_message-id
                                msgno     = is_message-number
                                msgv1     = is_message-message_v1
                                msgv2     = is_message-message_v2
                                msgv3     = is_message-message_v3
                                msgv4     = is_message-message_v4
                                time_stmp = CONV #( sy-datum && sy-uzeit )    ##OPERATOR[TIME_STMP]
                                probclass = COND #( WHEN sy-msgty = 'E' THEN '1'  "Very important
                                                    WHEN sy-msgty = 'W' THEN '2'  "Important
                                                    ELSE '3' ) ).                 "Medium

    ENDIF.

    "Add message to the log
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = mv_log_handle
        i_s_msg          = ls_log
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      "Error handling
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLS_FI_IC_LOGGER->PERSIST_DATA
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD persist_data.

    DATA(lt_handles) = VALUE bal_t_logh( ( mv_log_handle ) ).
    DATA(lt_log_num) = VALUE bal_t_lgnm( ).

    CALL FUNCTION 'BAL_DB_SAVE'           "#EC FB_RC
      EXPORTING
        i_t_log_handle   = lt_handles
        i_save_all       = abap_true
      IMPORTING
        e_new_lognumbers = lt_log_num
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF sy-subrc = 0 AND lt_log_num IS NOT INITIAL.
      mv_log_number = lt_log_num[ 1 ]-lognumber.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCLS_FI_IC_LOGGER->READ_LOG_NUMBER
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_LOG_NUM                     TYPE        BALOGNR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_log_number.

    rv_log_num = mv_log_number.

  ENDMETHOD.
ENDCLASS.
