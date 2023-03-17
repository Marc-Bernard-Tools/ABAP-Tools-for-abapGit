REPORT zabapgit_starter.

********************************************************************************
* abapGit Starter
*
* Simple tool that lets you define variants for running abapGit. Pick between
* developer or standalone version and starting up with repository list, last
* repository, or a specific repository based on key or SAP package.
*
* https://github.com/Marc-Bernard-Tools/ABAP-Tools-for-abapGit
* https://marcbernardtools.com/
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

CONSTANTS c_version TYPE string VALUE '1.0.0'.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME.
  SELECTION-SCREEN:
  COMMENT /1(77) sc_t001,
  COMMENT /1(77) sc_t002,
  COMMENT /1(77) sc_t003.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    p_devel RADIOBUTTON GROUP g1 DEFAULT 'X',
    p_stand RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:
    p_list RADIOBUTTON GROUP g2 DEFAULT 'X' USER-COMMAND repo,
    p_last RADIOBUTTON GROUP g2,
    p_repo RADIOBUTTON GROUP g2,
    p_pack TYPE tdevc-devclass MODIF ID pac.
SELECTION-SCREEN END OF BLOCK b2.

FORM screen.

  DATA lv_input TYPE abap_bool.

  LOOP AT SCREEN.
    IF screen-group1 = 'PAC'.
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

  sc_t001 = '- Save your favorite option as a variant'.
  sc_t002 = '- Create a transaction for your variant'.
  sc_t003 = '- Enjoy using a tcode for your favorite repository'.

AT SELECTION-SCREEN.

  PERFORM screen.

AT SELECTION-SCREEN OUTPUT.

  PERFORM screen.

START-OF-SELECTION.

  DATA:
    li_repo_srv TYPE REF TO zif_abapgit_repo_srv,
    li_repo     TYPE REF TO zif_abapgit_repo,
    lv_reason   TYPE string.

  IF p_list = abap_true.
    zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( || ).
  ELSEIF p_repo = abap_true.
    li_repo_srv = zcl_abapgit_repo_srv=>get_instance( ).
    li_repo_srv->get_repo_from_package(
      EXPORTING
        iv_package = p_pack
      IMPORTING
        ei_repo    = li_repo
        ev_reason  = lv_reason ).
    IF li_repo IS INITIAL.
      MESSAGE lv_reason TYPE 'S'.
      RETURN.
    ENDIF.
    zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( li_repo->get_key( ) ).
  ENDIF.

  IF p_devel = abap_true.
    SUBMIT zabapgit.
  ELSE.
    SUBMIT zabapgit_standalone.
  ENDIF.
