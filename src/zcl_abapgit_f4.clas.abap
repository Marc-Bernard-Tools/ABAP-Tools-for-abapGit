CLASS zcl_abapgit_f4 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      " Types must be data elements
      BEGIN OF ty_repo,
        key     TYPE bcablim_key,
        name    TYPE rstxtlg,
        package TYPE devclass,
        url     TYPE icf_stringurl,
      END OF ty_repo,
      BEGIN OF ty_user,
        key   TYPE usr02-bname,
        name  TYPE rstxtlg,
        email TYPE s_email,
      END OF ty_user,
      BEGIN OF ty_snapshot,
        key  TYPE rsddtimestmp,
        name TYPE rstxtlg,
      END OF ty_snapshot.

    CLASS-METHODS repository
      RETURNING
        VALUE(rv_result) TYPE bcablim_key.

    CLASS-METHODS user
      RETURNING
        VALUE(rv_result) TYPE usr02-bname.

    CLASS-METHODS snapshot
      RETURNING
        VALUE(rv_result) TYPE rsddtimestmp.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_f4 IMPLEMENTATION.


  METHOD repository.

    DATA:
      lt_list        TYPE zif_abapgit_persistence=>ty_repos,
      li_repo        TYPE REF TO zif_abapgit_repo,
      li_repo_online TYPE REF TO zif_abapgit_repo_online,
      ls_value       TYPE ty_repo,
      lt_value       TYPE STANDARD TABLE OF ty_repo WITH DEFAULT KEY,
      ls_return      TYPE ddshretval,
      lt_return      TYPE STANDARD TABLE OF ddshretval WITH DEFAULT KEY.

    FIELD-SYMBOLS <ls_list> LIKE LINE OF lt_list.

    TRY.
        lt_list = zcl_abapgit_persist_factory=>get_repo( )->list( ).

        LOOP AT lt_list ASSIGNING <ls_list>.
          li_repo = zcl_abapgit_repo_srv=>get_instance( )->get( <ls_list>-key ).
          CLEAR ls_value.
          ls_value-key     = <ls_list>-key.
          ls_value-name    = li_repo->get_name( ).
          ls_value-package = li_repo->get_package( ).
          IF li_repo->is_offline( ) = abap_true.
            ls_value-url = 'Offline'.
          ELSE.
            li_repo_online ?= li_repo.
            ls_value-url = li_repo_online->get_url( ).
          ENDIF.
          INSERT ls_value INTO TABLE lt_value.
        ENDLOOP.

        SORT lt_value BY name.
      CATCH cx_root.
        RETURN.
    ENDTRY.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'KEY'
        window_title    = 'Repository'
        value_org       = 'S'
      TABLES
        value_tab       = lt_value
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    LOOP AT lt_return INTO ls_return.
      rv_result = ls_return-fieldval.
    ENDLOOP.

  ENDMETHOD.


  METHOD snapshot.

    DATA:
      lt_content TYPE STANDARD TABLE OF zabapgit_snaps WITH KEY timestamp,
      ls_content LIKE LINE OF lt_content,
      ls_value   TYPE ty_snapshot,
      lt_value   TYPE STANDARD TABLE OF ty_snapshot WITH DEFAULT KEY,
      ls_return  TYPE ddshretval,
      lt_return  TYPE STANDARD TABLE OF ddshretval WITH DEFAULT KEY.

    SELECT * FROM zabapgit_snaps INTO TABLE lt_content.

    LOOP AT lt_content INTO ls_content.
      CLEAR ls_value.
      ls_value-key  = ls_content-timestamp.
      ls_value-name = ls_content-name.
      INSERT ls_value INTO TABLE lt_value.
    ENDLOOP.

    SORT lt_value DESCENDING BY key.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'KEY'
        window_title    = 'Snapshot'
        value_org       = 'S'
      TABLES
        value_tab       = lt_value
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    LOOP AT lt_return INTO ls_return.
      CALL FUNCTION 'CONVERSION_EXIT_TIMES_INPUT'
        EXPORTING
          input  = ls_return-fieldval
        IMPORTING
          output = rv_result.
    ENDLOOP.

  ENDMETHOD.


  METHOD user.

    DATA:
      lt_content TYPE zif_abapgit_persistence=>ty_contents,
      ls_content LIKE LINE OF lt_content,
      ls_value   TYPE ty_user,
      lt_value   TYPE STANDARD TABLE OF ty_user WITH DEFAULT KEY,
      ls_return  TYPE ddshretval,
      lt_return  TYPE STANDARD TABLE OF ddshretval WITH DEFAULT KEY.

    lt_content = zcl_abapgit_persistence_db=>get_instance( )->list_by_type( zcl_abapgit_persistence_db=>c_type_user ).

    LOOP AT lt_content INTO ls_content.
      CLEAR ls_value.
      ls_value-key  = ls_content-value.
      ls_value-name  = zcl_abapgit_env_factory=>get_user_record( )->get_name( ls_value-key ).
      ls_value-email = zcl_abapgit_env_factory=>get_user_record( )->get_email( ls_value-key ).
      INSERT ls_value INTO TABLE lt_value.
    ENDLOOP.

    SORT lt_value.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'KEY'
        window_title    = 'User'
        value_org       = 'S'
      TABLES
        value_tab       = lt_value
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    LOOP AT lt_return INTO ls_return.
      rv_result = ls_return-fieldval.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
