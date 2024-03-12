REPORT zabapgit_scheduler.

********************************************************************************
* abapGit Scheduler
*
* Tool to let you run the background logic for selected abapGit repositories.
* Simply, define a variant and schedule this program.
*
* Docs: https://docs.abapgit.org/user-guide/repo-settings/background-mode.html
*
* https://github.com/Marc-Bernard-Tools/ABAP-Tools-for-abapGit
*
* Copyright 2023 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
********************************************************************************
* MIT License
*
* Copyright (c) 2023 Marc Bernard
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*
* SPDX-License-Identifier: MIT
********************************************************************************

DATA gv_pack TYPE devclass.
DATA gv_repo TYPE c LENGTH 60.

CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

SELECTION-SCREEN BEGIN OF BLOCK sc_header WITH FRAME TITLE sc_titl0.
  SELECTION-SCREEN:
  SKIP,
  COMMENT /1(77) sc_txt0,
  SKIP,
  COMMENT /1(77) sc_txt1.
SELECTION-SCREEN END OF BLOCK sc_header.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK sc_repo WITH FRAME TITLE sc_titl1.
  SELECTION-SCREEN SKIP.
  PARAMETERS p_pack RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND repo.
  SELECT-OPTIONS s_pack FOR gv_pack MODIF ID pac.
  SELECTION-SCREEN SKIP.
  PARAMETERS p_repo RADIOBUTTON GROUP g1.
  SELECT-OPTIONS s_repo FOR gv_repo MODIF ID rep.
  SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK sc_repo.

FORM screen.

  DATA lv_input TYPE abap_bool.

  LOOP AT SCREEN.
    IF screen-group1 = 'PAC'.
      lv_input = boolc( p_pack = abap_true ).
    ELSEIF screen-group1 = 'REP'.
      lv_input = boolc( p_repo = abap_true ).
    ELSE.
      lv_input = abap_true.
    ENDIF.

    IF lv_input = abap_true.
      screen-input = '1'.
    ELSE.
      screen-input = '0'.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.

INITIALIZATION.

  sc_titl0 = 'Description'.
  sc_txt0  = 'Run the background logic for selected abapGit repositories.'.
  sc_txt1  = 'Simply, define a variant and schedule this program.'.

  sc_titl1 = 'Repository Selection'.

FORM run.

  DATA: lo_per        TYPE REF TO zcl_abapgit_persist_background,
        lo_repo       TYPE REF TO zcl_abapgit_repo_online,
        lt_list       TYPE zcl_abapgit_persist_background=>ty_background_keys,
        li_background TYPE REF TO zif_abapgit_background,
        li_log        TYPE REF TO zif_abapgit_log,
        lx_error      TYPE REF TO zcx_abapgit_exception,
        lv_error      TYPE string,
        lv_package    TYPE devclass,
        lv_repo_name  TYPE string.

  FIELD-SYMBOLS <ls_list> LIKE LINE OF lt_list.

  TRY.
      zcl_abapgit_background=>enqueue( ).
    CATCH zcx_abapgit_exception.
      WRITE: / 'Another instance of the program is already running'.
      RETURN.
  ENDTRY.

  TRY.
      CREATE OBJECT lo_per.
      lt_list = lo_per->list( ).

    CATCH zcx_abapgit_exception INTO lx_error.
      lv_error = lx_error->get_text( ).
      WRITE / lv_error.
  ENDTRY.

  WRITE / 'Background mode'.

  LOOP AT lt_list ASSIGNING <ls_list>.
    TRY.
        lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( <ls_list>-key ).

        lv_package   = lo_repo->get_package( ).
        lv_repo_name = lo_repo->get_name( ).

        CASE abap_true.
          WHEN p_pack.
            CHECK lv_package IN s_pack.
          WHEN p_repo.
            CHECK lv_repo_name IN s_repo.
        ENDCASE.

        WRITE: / <ls_list>-method, lv_repo_name.

        zcl_abapgit_login_manager=>set(
          iv_uri      = lo_repo->get_url( )
          iv_username = <ls_list>-username
          iv_password = <ls_list>-password ).

        CREATE OBJECT li_log TYPE zcl_abapgit_log.
        CREATE OBJECT li_background TYPE (<ls_list>-method).

        li_background->run(
          io_repo     = lo_repo
          ii_log      = li_log
          it_settings = <ls_list>-settings ).

        li_log->add_success( |Repo { lv_repo_name } processed successfully| ).

        " Clear auth buffer to allow different user/password per repository in background mode
        zcl_abapgit_login_manager=>clear( ).

      CATCH zcx_abapgit_exception INTO lx_error.
        li_log->add_exception( lx_error ).
    ENDTRY.

    zcl_abapgit_log_viewer=>write_log( li_log ).
  ENDLOOP.

  IF lines( lt_list ) = 0.
    WRITE / 'Nothing configured'.
  ENDIF.

  zcl_abapgit_background=>dequeue( ).

ENDFORM.

AT SELECTION-SCREEN.

  PERFORM screen.

AT SELECTION-SCREEN OUTPUT.

  PERFORM screen.

START-OF-SELECTION.

  IF p_pack = abap_true AND s_pack IS INITIAL.
    MESSAGE e000(oo) WITH 'Select at least one package'.
  ELSEIF p_repo = abap_true AND s_repo IS INITIAL.
    MESSAGE e000(oo) WITH 'Enter at least one repository name'.
  ENDIF.

  PERFORM run.
