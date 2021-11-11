REPORT yteste.


PARAMETERS:
  p_bp     TYPE bus_joel_main-change_number OBLIGATORY,
  p_person TYPE bus050dfld-partner2 OBLIGATORY.

START-OF-SELECTION .


  DATA:
    lt_return TYPE bapiret2_t .

  IF ( p_bp IS NOT INITIAL ) AND
     ( p_person IS NOT INITIAL ) .

    IF ( sy-uname EQ 'EXEDEJ' ) .
      BREAK-POINT .
    ENDIF.

    " check relationship
    SELECT relnr, partner1, partner2, date_to, date_from, reltyp, dftval
      FROM but050
      INTO TABLE @DATA(lt_data)
     WHERE partner1 EQ @p_bp
       AND partner2 EQ @p_person .

    IF ( sy-subrc EQ 0 ) .

      DATA(lv_businesspartner1)     = CONV bapibus1006_head-bpartner( p_bp ) .
      DATA(lv_businesspartner2)     = CONV bapibus1006_head-bpartner( p_person ) .
      DATA(lv_relationshipcategory) = VALUE but050-reltyp( lt_data[ 1 ]-reltyp OPTIONAL ) .
      DATA(lv_validfromdate)        = VALUE but050-date_from( lt_data[ 1 ]-date_from OPTIONAL ) .
      DATA(lv_validuntildate)       = VALUE but050-date_to( lt_data[ 1 ]-date_to OPTIONAL ) .

*      CALL FUNCTION 'BAPI_BUPR_RELATIONSHIP_DELETE'
*        EXPORTING
*          businesspartner1     = lv_businesspartner1
*          businesspartner2     = lv_businesspartner2
*          relationshipcategory = lv_relationshipcategory
*         DIFFERENTIATIONTYPEVALUE       =
*        TABLES
*          return               = lt_return.

      CALL FUNCTION 'BAPI_BUPR_RELATIONSHIP_REMOVE'
        EXPORTING
          businesspartner1     = lv_businesspartner1
          businesspartner2     = lv_businesspartner2
          relationshipcategory = lv_relationshipcategory
          validfromdate        = lv_validfromdate
          validuntildate       = lv_validuntildate
*         DIFFERENTIATIONTYPEVALUE       =
        TABLES
          return               = lt_return.


      IF ( line_exists( lt_return[ type = 'E' ] ) ) .
      ELSE .
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' .
      ENDIF .

      IF ( lines( lt_return ) GT 0 ) .

        cl_demo_output=>display( lt_return ) .

      ELSE .

        MESSAGE i000(>0) WITH 'It has been deleted as successful.'  DISPLAY LIKE 'I' .

      ENDIF .

    ELSE.

      MESSAGE w000(>0) WITH 'It has no data (BUT050).' DISPLAY LIKE 'I' .

    ENDIF .

  ENDIF .
