class ZIWF_CL_DB_SELECT_CARGOLINK definition
  public
  final
  create public .

public section.

  class-methods GET
    importing
      !IV_TABLE type TABNAME16 optional
      !IV_WHERE type STRING optional
      !IV_LIMIT type I default '3'
    exporting
      !ET_RETURN type BAPIRET2_T .
  methods START_CONNECTION
    exporting
      !EV_ERROR type STRING
      value(EO_SQL_CONNECTION) type ref to CL_SQL_CONNECTION
      value(EO_SQL_STATEMENT) type ref to CL_SQL_STATEMENT .
  methods GET_DATA
    importing
      !IV_WHERE_CLAUSE_STATEMENT type STRING
      !IO_SQL_STATEMENT type ref to CL_SQL_STATEMENT
    exporting
      !EV_ERROR type STRING
    changing
      !CO_DATA type DATA
    returning
      value(RTO_SQL_RESULT_SET) type ref to CL_SQL_RESULT_SET .
  methods CLOSE
    importing
      !IO_SQL_CONNECTION type ref to CL_SQL_CONNECTION
      !IO_SQL_RESULT_SET type ref to CL_SQL_RESULT_SET
    exporting
      !EV_ERROR type STRING .
protected section.
private section.

  methods MAKE_DB_CONNECTION
    exporting
      !EV_ERROR type STRING
    returning
      value(RT_SQL_CONNECTION) type ref to CL_SQL_CONNECTION
    raising
      CX_SQL_EXCEPTION .
  methods INIT_SQL_STATEMENT
    importing
      !IO_SQL_CONNECTION type ref to CL_SQL_CONNECTION
    exporting
      !EV_ERROR type STRING
    returning
      value(RTO_SQL_STATEMENT) type ref to CL_SQL_STATEMENT .
  methods ISSUE_NATIVE_SQL_CALL
    importing
      !IV_WHERE_CLAUSE_STATEMENT type STRING
      !IO_SQL_STATEMENT type ref to CL_SQL_STATEMENT
    returning
      value(RTO_SQL_RESULT_SET) type ref to CL_SQL_RESULT_SET
    raising
      CX_SQL_EXCEPTION .
  methods ASSIGN_TARGET_RESULT
    importing
      !IO_SQL_RESULT_SET type ref to CL_SQL_RESULT_SET
    changing
      !CO_DATA type ref to DATA
    raising
      CX_PARAMETER_INVALID .
  methods CLOSE_QUERY
    importing
      !IO_SQL_RESULT_SET type ref to CL_SQL_RESULT_SET .
  methods CLOSE_DB_CONNECTION
    importing
      !IO_SQL_CONNECTION type ref to CL_SQL_CONNECTION
    exporting
      !EV_ERROR type STRING .
ENDCLASS.



CLASS ZIWF_CL_DB_SELECT_CARGOLINK IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZIWF_CL_DB_SELECT_CARGOLINK->ASSIGN_TARGET_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SQL_RESULT_SET              TYPE REF TO CL_SQL_RESULT_SET
* | [<-->] CO_DATA                        TYPE REF TO DATA
* | [!CX!] CX_PARAMETER_INVALID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD assign_target_result.

    io_sql_result_set->set_param_table(
      EXPORTING
        itab_ref = co_data ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZIWF_CL_DB_SELECT_CARGOLINK->CLOSE_DB_CONNECTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SQL_CONNECTION              TYPE REF TO CL_SQL_CONNECTION
* | [<---] EV_ERROR                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD close_db_connection.

    CLEAR ev_error.

    TRY.
        io_sql_connection->close( ).
      CATCH cx_sql_exception INTO DATA(lo_error).
        ev_error = lo_error->get_text( ).
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZIWF_CL_DB_SELECT_CARGOLINK->CLOSE_QUERY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SQL_RESULT_SET              TYPE REF TO CL_SQL_RESULT_SET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD close_query.

    io_sql_result_set->close( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZIWF_CL_DB_SELECT_CARGOLINK->INIT_SQL_STATEMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SQL_CONNECTION              TYPE REF TO CL_SQL_CONNECTION
* | [<---] EV_ERROR                       TYPE        STRING
* | [<-()] RTO_SQL_STATEMENT              TYPE REF TO CL_SQL_STATEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD init_sql_statement.

    CLEAR ev_error.

    IF io_sql_connection IS BOUND.
      rto_sql_statement = io_sql_connection->create_statement( ).
    ELSE.
      ev_error = TEXT-001.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZIWF_CL_DB_SELECT_CARGOLINK->ISSUE_NATIVE_SQL_CALL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_WHERE_CLAUSE_STATEMENT      TYPE        STRING
* | [--->] IO_SQL_STATEMENT               TYPE REF TO CL_SQL_STATEMENT
* | [<-()] RTO_SQL_RESULT_SET             TYPE REF TO CL_SQL_RESULT_SET
* | [!CX!] CX_SQL_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD issue_native_sql_call.

    io_sql_statement->execute_query(
      EXPORTING
        statement             =  iv_where_clause_statement
        hold_cursor           = space
      RECEIVING
        result_set            = rto_sql_result_set
    ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZIWF_CL_DB_SELECT_CARGOLINK->MAKE_DB_CONNECTION
* +-------------------------------------------------------------------------------------------------+
* | [<---] EV_ERROR                       TYPE        STRING
* | [<-()] RT_SQL_CONNECTION              TYPE REF TO CL_SQL_CONNECTION
* | [!CX!] CX_SQL_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD make_db_connection.

    DATA:
      lv_con_name TYPE dbcon_name VALUE 'CARGOLNK' .

    CLEAR ev_error.
    "Get DB
    rt_sql_connection = cl_sql_connection=>get_connection( con_name = lv_con_name ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZIWF_CL_DB_SELECT_CARGOLINK->CLOSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SQL_CONNECTION              TYPE REF TO CL_SQL_CONNECTION
* | [--->] IO_SQL_RESULT_SET              TYPE REF TO CL_SQL_RESULT_SET
* | [<---] EV_ERROR                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD close.

    CLEAR ev_error.

    IF io_sql_connection IS BOUND AND
       io_sql_result_set IS BOUND.

      TRY.
          close_query( EXPORTING io_sql_result_set = io_sql_result_set ).
          close_db_connection( EXPORTING io_sql_connection = io_sql_connection ).
        CATCH cx_sql_exception INTO DATA(lo_error).
          ev_error = lo_error->get_text( ).
      ENDTRY.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZIWF_CL_DB_SELECT_CARGOLINK=>GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TABLE                       TYPE        TABNAME16(optional)
* | [--->] IV_WHERE                       TYPE        STRING(optional)
* | [--->] IV_LIMIT                       TYPE        I (default ='3')
* | [<---] ET_RETURN                      TYPE        BAPIRET2_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get.

    TYPES:
      BEGIN OF ty_result,
        transport      TYPE zsagshoved-transport,
        dm7_opg_status TYPE zsagshoved-dm7_opg_status,
        dm7_end_status TYPE zsagshoved-dm7_end_status,
        afdeling       TYPE zsagshoved-afdeling,
        dm7_ekspeddato TYPE zsagshoved-dm7_ekspeddato,
      END OF ty_result.

    DATA:
*      lt_result                 TYPE STANDARD TABLE OF ty_result,
      lr_data                   TYPE REF TO data,
      lv_where_clause_statement TYPE string,
      lt_lines                  TYPE trtexts.

    CLEAR et_return .

    DATA(lo_db_select_cargolink) = NEW ziwf_cl_db_select_cargolink( ).

    lo_db_select_cargolink->start_connection( IMPORTING ev_error          = DATA(lv_error)
                                                        eo_sql_connection = DATA(lo_sql_connection)
                                                        eo_sql_statement  = DATA(lo_sql_statement) ) .
    IF ( iv_table IS INITIAL ) .

      DATA(lt_tables) =
        VALUE swf_logtabs( ( 'ZCPH_SAGSHOVED' )
                           ( 'ZANT_SAGSHOVED' )
                           ( 'ZMAL_SAGSHOVED' )
                           ( 'ZSPAN_SAGSHOVED' )
                           ( 'ZPAR_SAGSHOVED' )
                           ( 'ZOSLO_SAGSHOVED' )
                           ( 'ZHEL_SAGSHOVED' )
                           ( 'ZROT_SAGSHOVED' )
                           ( 'ZDUB_SAGSHOVED' )
                           ( 'ZHAR_SAGSHOVED' )
                           ( 'ZWAW_SAGSHOVED' )
                           ( 'ZFRAN_SAGSHOVED' )
                           ( 'ZSWIT_SAGSHOVED' )
                           ( 'ZTURK_SAGSHOVED' )
                           ( 'ZAUST_SAGSHOVED' )
                           ( 'ZROMA_SAGSHOVED' )
                           ( 'ZPORT_SAGSHOVED' )
                           ( 'ZSLOK_SAGSHOVED' )
                           ( 'ZHUNG_SAGSHOVED' )
                           ( 'ZPRAG_SAGSHOVED' )
                           ( 'ZTAL_SAGSHOVED' )
                           ( 'ZVIL_SAGSHOVED' )
                           ( 'ZRIG_SAGSHOVED' )
                           ( 'ZRIG_SAGSHOVED' ) ) .

    ELSE .

      lt_tables =
        VALUE swf_logtabs( ( iv_table ) ) .

    ENDIF .

    LOOP AT lt_tables INTO DATA(ls_table) .

      DATA(lv_where) =
        COND char100( WHEN iv_where IS NOT INITIAL
                      THEN iv_where
                      ELSE '' ) .

      lv_where_clause_statement = |SELECT * |          &&
                                  |FROM { ls_table } | &&
                                  |{ lv_where } |      &&
                                  |LIMIT { iv_limit }| &&
                                  |. | .

*      GET REFERENCE OF ( ls_table ) INTO lr_data.

      CREATE DATA lr_data TYPE REF TO data .

      CLEAR lv_error .

      DATA(lo_sql_result_set) =
            lo_db_select_cargolink->get_data( EXPORTING iv_where_clause_statement = lv_where_clause_statement
                                                        io_sql_statement          = lo_sql_statement
                                              IMPORTING ev_error                  = lv_error
                                              CHANGING  co_data                   = lr_data ) .

      IF ( lv_error IS NOT INITIAL ) .

        APPEND VALUE bapiret2( type       = if_xo_const_message=>error
                               id         = '>0'
                               number     = '000'
                               message    = |Data has not been found on table: { ls_table }| ) TO et_return .
*                               message_v1 = 'Data has not been found on table: '
*                               message_v2 = ls_table ) TO et_return .

*        CALL FUNCTION 'TR_SPLIT_TEXT'
*          EXPORTING
*            iv_text  = CONV char200( lv_error )
*            iv_len   = 50
*          IMPORTING
*            et_lines = lt_lines.

        APPEND VALUE bapiret2( type       = if_xo_const_message=>error
                               id         = '>0'
                               number     = '000'
                               message    = lv_error ) TO et_return .
*                               message_v1 = VALUE #( lt_lines[ 1 ] OPTIONAL )
*                               message_v2 = VALUE #( lt_lines[ 2 ] OPTIONAL )
*                               message_v3 = VALUE #( lt_lines[ 3 ] OPTIONAL )
*                               message_v4 = VALUE #( lt_lines[ 4 ] OPTIONAL ) ) TO et_return .
      ELSE .
        APPEND VALUE bapiret2( type       = if_xo_const_message=>info
                               id         = '>0'
                               number     = '000'
                               message    = |Data has been found on table: { ls_table }| ) TO et_return .
*                               message_v1 = 'Data has been found on table: '
*                               message_v2 = ls_table ) TO et_return .

        ASSIGN lr_data->* TO FIELD-SYMBOL(<table>) .
        IF ( <table> IS ASSIGNED ) AND
           ( <table> IS NOT INITIAL ).
          UNASSIGN <table> .
        ENDIF .

      ENDIF .

    ENDLOOP .

    lo_db_select_cargolink->close( EXPORTING io_sql_connection = lo_sql_connection
                                             io_sql_result_set = lo_sql_result_set ) .

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZIWF_CL_DB_SELECT_CARGOLINK->GET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_WHERE_CLAUSE_STATEMENT      TYPE        STRING
* | [--->] IO_SQL_STATEMENT               TYPE REF TO CL_SQL_STATEMENT
* | [<---] EV_ERROR                       TYPE        STRING
* | [<-->] CO_DATA                        TYPE        DATA
* | [<-()] RTO_SQL_RESULT_SET             TYPE REF TO CL_SQL_RESULT_SET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_data.

    CLEAR ev_error.

    TRY.
        rto_sql_result_set = issue_native_sql_call(
                              EXPORTING
                                iv_where_clause_statement = iv_where_clause_statement
                                io_sql_statement          = io_sql_statement  ).

        assign_target_result(
          EXPORTING
            io_sql_result_set = rto_sql_result_set
          CHANGING
        co_data           = co_data ).

        rto_sql_result_set->next_package( ).

      CATCH cx_sql_exception INTO DATA(lo_error).
        ev_error = lo_error->get_text( ).
      CATCH cx_parameter_invalid_type INTO DATA(lo_error_type).
        ev_error = lo_error_type->get_text( ).
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZIWF_CL_DB_SELECT_CARGOLINK->START_CONNECTION
* +-------------------------------------------------------------------------------------------------+
* | [<---] EV_ERROR                       TYPE        STRING
* | [<---] EO_SQL_CONNECTION              TYPE REF TO CL_SQL_CONNECTION
* | [<---] EO_SQL_STATEMENT               TYPE REF TO CL_SQL_STATEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD start_connection.

    CLEAR ev_error.

    TRY.
        eo_sql_connection = make_db_connection( ).
        eo_sql_statement = init_sql_statement(
                            EXPORTING
                              io_sql_connection = eo_sql_connection ).
      CATCH cx_sql_exception INTO DATA(lo_error).
        ev_error = lo_error->get_text( ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
