;;; eicq-meta.el --- User meta info code for Eicq

;; Copyright (C) 2002,03,04 Steve Youngs

;; RCS: $Id$
;; Author:        Steve Youngs <steve@youngs.au.com>
;; Maintainer:    Steve Youngs <steve@youngs.au.com>
;; Created:       2002-10-02
;; Last-Modified: <2004-05-30 11:14:17 (steve)>
;; Homepage:      http://eicq.sf.net/
;; Keywords:      comm ICQ

;; This file is part of Eicq.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the author nor the names of any contributors
;;    may be used to endorse or promote products derived from this
;;    software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defgroup eicq-meta nil
  "User info stored in ICQ server.
Run `eicq-update-meta-info' after changing any of these variables."
  :group 'eicq)

(defcustom eicq-user-meta-nickname "e-i-c-q"
  "*Your nickname stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable.
ICQ server refuses any string containing substring \"icq\"."
  :group 'eicq-meta)

(defcustom eicq-user-meta-firstname "XEmacs"
  "*Your first name stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable.
ICQ server refuses any string containing substring \"icq\"."
  :group 'eicq-meta)

(defcustom eicq-user-meta-lastname "Linux"
  "*Your last name stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable.
ICQ server refuses any string containing substring \"icq\"."
  :group 'eicq-meta)

(defcustom eicq-user-meta-primary-email "emacs@home.com"
  "*Your primary email address stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable.
ICQ server refuses any string containing substring \"icq\"."
  :group 'eicq-meta)

(defcustom eicq-user-meta-secondary-email "eicq@home.com"
  "*Your secondary email address stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-old-email "eicq@home.com"
  "*Your old email address stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-city nil
  "*Your city stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-state nil
  "*Your state stored on the ICQ server.
We're talking 'address' here, not online state.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-phone nil
  "*Your phone number stored on the ICQ server.
Do you really want to divulge this information?
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-fax nil
  "*Your fax number stored on the ICQ server.
Do you really want to divulge this information?
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-street nil
  "*Your street name and number stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-cell-phone nil
  "*Your cell phone number stored on the ICQ server.
Do you really want to divulge this information?
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-zipcode nil
  "*Your zip code/postal code stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-country nil
  "*Your country stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-web-aware t
  "*Set this to non-nil if you want your presence know on the web."
  :type 'boolean
  :group 'eicq-meta)

(defcustom eicq-user-meta-hide-ip nil
  "*Set to non-nil if you want to hide your IP.
Run `eicq-update-meta-info' after modifying this variable."
  :type 'boolean
  :group 'eicq-meta)

(defcustom eicq-user-meta-authorization t
  "*Authorization needed to add you to others' contact lists..
Run `eicq-update-meta-info' after modifying this variable."
  :type 'boolean
  :group 'eicq-meta)

(defcustom eicq-user-meta-about
  "Give a man a new Emacs command,
  and he can hack for a night;
Teach a man to make new Emacs commands,
  and he can hack for a life time."
  "*User 'about' info stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-homepage 
  "http://eicq.sf.net/"
  "*User homepage stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-age 65535
  "*Your age stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable.
65535 = not entered."
  :group 'eicq-meta)

(defcustom eicq-user-meta-sex 'not-entered
  "*Your sex stored on the ICQ server.
No, it's not an invitation.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta
  :type '(choice (item male) (item female)
                 (item not-entered)))

(defcustom eicq-user-meta-birth-year nil
  "*Your birth year (YY) stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-birth-month nil
  "*Your birth month (MM) stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-birth-day nil
  "*Your birth day (DD) stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-language-1 0
  "*User 1st language stored on the ICQ server.
Possible values are:

 0 Unspecified 7 Chinese   17 French     27 Japanese   36 Portuguese 46 Tagalog
55 Afrikaans   8 Croatian  18 Gaelic     28 Khmer      37 Romanian   47 Tatar
58 Albanian    9 Czech     19 German     29 Korean     38 Russian    48 Thai
 1 Arabic     10 Danish    20 Greek      30 Lao        39 Serbian    49 Turkish
59 Armenian   11 Dutch     21 Hebrew     31 Latvian    40 Slovak     50 Ukrainian
 2 Bhojpuri   12 English   22 Hindi      32 Lithuanian 41 Slovenian  51 Urdu
 3 Bulgarian  13 Esperanto 23 Hungarian  33 Malay      42 Somali     52 Vietnamese
 4 Burmese    14 Estonian  24 Icelandic  34 Norwegian  43 Spanish    53 Yiddish
 5 Cantonese  15 Farsi     25 Indonesian 57 Persian    44 Swahili    54 Yoruba
 6 Catalan    16 Finnish   26 Italian    35 Polish     45 Swedish

Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-language-2 0
  "*User 2nd language stored on the ICQ server.
Possible values are:

 0 Unspecified 7 Chinese   17 French     27 Japanese   36 Portuguese 46 Tagalog
55 Afrikaans   8 Croatian  18 Gaelic     28 Khmer      37 Romanian   47 Tatar
58 Albanian    9 Czech     19 German     29 Korean     38 Russian    48 Thai
 1 Arabic     10 Danish    20 Greek      30 Lao        39 Serbian    49 Turkish
59 Armenian   11 Dutch     21 Hebrew     31 Latvian    40 Slovak     50 Ukrainian
 2 Bhojpuri   12 English   22 Hindi      32 Lithuanian 41 Slovenian  51 Urdu
 3 Bulgarian  13 Esperanto 23 Hungarian  33 Malay      42 Somali     52 Vietnamese
 4 Burmese    14 Estonian  24 Icelandic  34 Norwegian  43 Spanish    53 Yiddish
 5 Cantonese  15 Farsi     25 Indonesian 57 Persian    44 Swahili    54 Yoruba
 6 Catalan    16 Finnish   26 Italian    35 Polish     45 Swedish

Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-language-3 0
  "*User 3rd language stored on the ICQ server.
Possible values are:

 0 Unspecified 7 Chinese   17 French     27 Japanese   36 Portuguese 46 Tagalog
55 Afrikaans   8 Croatian  18 Gaelic     28 Khmer      37 Romanian   47 Tatar
58 Albanian    9 Czech     19 German     29 Korean     38 Russian    48 Thai
 1 Arabic     10 Danish    20 Greek      30 Lao        39 Serbian    49 Turkish
59 Armenian   11 Dutch     21 Hebrew     31 Latvian    40 Slovak     50 Ukrainian
 2 Bhojpuri   12 English   22 Hindi      32 Lithuanian 41 Slovenian  51 Urdu
 3 Bulgarian  13 Esperanto 23 Hungarian  33 Malay      42 Somali     52 Vietnamese
 4 Burmese    14 Estonian  24 Icelandic  34 Norwegian  43 Spanish    53 Yiddish
 5 Cantonese  15 Farsi     25 Indonesian 57 Persian    44 Swahili    54 Yoruba
 6 Catalan    16 Finnish   26 Italian    35 Polish     45 Swedish

Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-city nil
  "*User work city stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-state nil
  "*User work state stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-phone nil
  "*User work phone number stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-fax nil
  "*User work fax number stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-address nil
  "*User work address stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-company nil
  "*User company stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-department nil
  "*User work department stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-position nil
  "*User work position stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

(defcustom eicq-user-meta-work-homepage "http://eicq.sf.net/"
  "*User work homepage stored on the ICQ server.
Run `eicq-update-meta-info' after modifying this variable."
  :group 'eicq-meta)

;;; Internal variables

(autoload 'eicq-pack "eicq")
(autoload 'eicq-int-bin "eicq")
(autoload 'eicq-alias-bin "eicq")
(autoload 'eicq-int-byte "eicq")
(autoload 'eicq-log-info "eicq-log")
(autoload 'eicq-log-debug "eicq-log")
(autoload 'eicq-bin-hex "eicq")
(autoload 'eicq-bin-pretty-hex "eicq")
(autoload 'eicq-bin-int "eicq")
(autoload 'eicq-byte-int "eicq")
(autoload 'eicq-decode-string "eicq")
(autoload 'eicq-send "eicq")

(defun eicq-pack-meta-user-change-password (password)
  "Pack meta user change password packet 064a:042e.
ALIAS is the alias/uin to query info for."
  (eicq-pack
   "\x4a\x06"
   "\x2e\x04"
   (eicq-int-bin (length password))
   password))

(defun eicq-pack-meta-user-query (alias)
  "Pack meta user query packet 064a:04b0.
ALIAS is the alias/uin to query info for."
  (eicq-pack
   "\x4a\x06"
   "\xb0\x04"
   (eicq-alias-bin alias)))

(defvar eicq-country-code
  '((65535 . "not entered")
    (93 . "Afghanistan")
    (355 . "Albania")
    (213 . "Algeria")
    (376 . "Andorra")
    (244 . "Angola")
    (672 . "Antarctic Aus Territory")
    (599 . "Antilles")
    (54 . "Argentina")
    (374 . "Armenia")
    (297 . "Aruba")
    (247 . "Ascension Island")
    (61 . "Australia")
    (43 . "Austria")
    (994 . "Azerbaijan")
    (351 . "Azores")
    (973 . "Bahrain")
    (890 . "Bangladesh")
    (809 . "Barbados")
    (257 . "Barundi")
    (375 . "Belarus")
    (32 . "Belgium")
    (229 . "Belize")
    (501 . "Belize")
    (975 . "Bhutan")
    (591 . "Bolivia")
    (387 . "Bosnia Hercegovina")
    (267 . "Botswana")
    (55 . "Brazil")
    (673 . "Brunei Darussalam")
    (226 . "Bukina Faso")
    (359 . "Bulgaria")
    (855 . "Cambodia")
    (237 . "Cameroon")
    (107 . "Canada")
    (238 . "Cape Verde Islands")
    (236 . "Central African Republic")
    (235 . "Chad")
    (56 . "Chile")
    (86 . "China")
    (672 . "Christmas Island")
    (672 . "Cocos Island")
    (57 . "Columbia")
    (269 . "Comoros")
    (242 . "Congo")
    (682 . "Cook Islands")
    (506 . "Costa Rica")
    (225 . "Cote d'Ivorie")
    (385 . "Croatia")
    (53 . "Cuba")
    (357 . "Cyprus")
    (42 . "Czech Republic")
    (45 . "Denmark")
    (253 . "Djibouti")
    (593 . "Ecuador")
    (20 . "Egypt")
    (503 . "El Salvador")
    (240 . "Equatorial Guinea")
    (291 . "Eritrea")
    (372 . "Estonia")
    (251 . "Ethiopia")
    (500 . "Falkland Islands")
    (298 . "Faroe Islands")
    (679 . "Fiji")
    (358 . "Finland")
    (33 . "France")
    (594 . "French Guiana")
    (689 . "French Polynesia")
    (241 . "Gabon")
    (220 . "Gambia")
    (49 . "Germany")
    (233 . "Ghana")
    (350 . "Gibraltar")
    (30 . "Greece")
    (299 . "Greenland")
    (590 . "Guadeloupe")
    (671 . "Guam")
    (502 . "Guatemala")
    (245 . "Guinea - Bissau")
    (224 . "Guinea")
    (592 . "Guyana")
    (509 . "Haiti")
    (504 . "Honduras")
    (852 . "Hong Kong")
    (36 . "Hungary")
    (354 . "Iceland")
    (91 . "India")
    (62 . "Indonesia")
    (98 . "Iran")
    (964 . "Iraq")
    (353 . "Ireland Republic of")
    (972 . "Israel")
    (39 . "Italy")
    (225 . "Ivory Coast see Cote d'Ivorie")
    (81 . "Japan")
    (962 . "Jordan")
    (7 . "Kazakhstan")
    (254 . "Kenya")
    (7 . "Kirghizstan")
    (686 . "Kiribati")
    (850 . "North Korea")
    (82 . "South Korea")
    (965 . "Kuwait")
    (856 . "Laos")
    (371 . "Latvia")
    (961 . "Lebanon")
    (266 . "Lesotho")
    (231 . "Liberia")
    (370 . "Lithuania")
    (352 . "Luxembourg")
    (218 . "Lybia")
    (853 . "Macao")
    (389 . "Macedonia")
    (261 . "Madagascar")
    (265 . "Malawi")
    (60 . "Malaysia")
    (960 . "Maldives")
    (223 . "Mali")
    (356 . "Malta")
    (692 . "Marshall Islands")
    (596 . "Martinique")
    (222 . "Mauritania")
    (230 . "Mauritius")
    (269 . "Mayotte")
    (52 . "Mexico")
    (691 . "Micronesia")
    (373 . "Moldovia")
    (976 . "Mongolia")
    (212 . "Morocco")
    (258 . "Mozanbique")
    (95 . "Myanmar (Burma)")
    (264 . "Namibia")
    (977 . "Napal")
    (674 . "Nauru")
    (31 . "Netherlands (Holland)")
    (599 . "Netherlands Antilles")
    (687 . "New Caledonia")
    (505 . "Nicaragua")
    (227 . "Niger")
    (234 . "Nigeria")
    (47 . "Norway")
    (968 . "Oman")
    (92 . "Pakistan")
    (507 . "Panama")
    (675 . "Papua New Guinea")
    (595 . "Paraguay")
    (51 . "Peru")
    (63 . "Philippines")
    (649 . "Pitcain Island")
    (48 . "Poland")
    (361 . "Portugal")
    (974 . "Qatar")
    (40 . "Romania")
    (7 . "Russia")
    (250 . "Rwanda")
    (685 . "Samoa (USA)")
    (685 . "Samoa Western")
    (378 . "San Marino")
    (966 . "Saudi Arabia")
    (221 . "Senegal")
    (248 . "Seychelles")
    (232 . "Sierra Leone")
    (65 . "Singapore")
    (42 . "Slovakia")
    (386 . "Slovenia")
    (677 . "Solom Islands")
    (252 . "Somalia")
    (27 . "South Africa")
    (34 . "Spain")
    (94 . "Sri Lanka")
    (290 . "St Helena")
    (249 . "Sudan")
    (597 . "Surinam")
    (268 . "Swaziland")
    (46 . "Sweden")
    (41 . "Switzerland")
    (963 . "Syria")
    (886 . "Taiwan")
    (7 . "Tajikistan")
    (255 . "Tanzania")
    (66 . "Thailand")
    (228 . "Togo")
    (676 . "Tongo")
    (216 . "Tunisia")
    (90 . "Turkey")
    (7 . "Turkmenistan")
    (688 . "Tuvalu")
    (1 . "USA")
    (256 . "Uganda")
    (380 . "Ukraine")
    (971 . "United Arab Emirates")
    (44 . "United Kingdom")
    (598 . "Uraguay")
    (7 . "Uzbekistan")
    (678 . "Vanuatu")
    (58 . "Venezuela")
    (84 . "Vietnam")
    (967 . "Yemen")
    (381 . "Yugoslavia")
    (243 . "Zaire")
    (260 . "Zambia")
    (263 . "Zimbabwe"))
  "ISO 3166 country codes.")

(defun eicq-pack-meta-user-update-general ()
  "Pack meta user packet 064a:03e8.
`M-x customize-group RET eicq-info' before this."
  (eicq-pack
   "\x4a\x06"
   "\xe8\x03"
   (eicq-int-bin (length eicq-user-meta-nickname))
   eicq-user-meta-nickname
   (eicq-int-bin (length eicq-user-meta-firstname))
   eicq-user-meta-firstname
   (eicq-int-bin (length eicq-user-meta-lastname))
   eicq-user-meta-lastname
   (eicq-int-bin (length eicq-user-meta-primary-email))
   eicq-user-meta-primary-email
   (eicq-int-bin (length eicq-user-meta-secondary-email))
   eicq-user-meta-secondary-email
   (eicq-int-bin (length eicq-user-meta-old-email))
   eicq-user-meta-old-email
   (eicq-int-bin (length eicq-user-meta-city))
   eicq-user-meta-city
   (eicq-int-bin (length eicq-user-meta-state))
   eicq-user-meta-state
   (eicq-int-bin (length eicq-user-meta-phone))
   eicq-user-meta-phone
   (eicq-int-bin (length eicq-user-meta-fax))
   eicq-user-meta-fax
   (eicq-int-bin (length eicq-user-meta-street))
   eicq-user-meta-street
   (eicq-int-bin (length eicq-user-meta-cell-phone))
   eicq-user-meta-cell-phone
   ;; zip code? only accept valid values
   "\x00\x00\x00\x00"
   (eicq-int-bin (car (rassoc eicq-user-meta-country eicq-country-code)))))

(defun eicq-pack-meta-user-security ()
  "Pack meta user packet 064a:0424."
  (eicq-pack
   "\x4a\x06"
   "\x24\x04"
   (if eicq-user-meta-authorization
       "\x00"
     "\x01")
   (if eicq-user-meta-web-aware
       "\x01" 
     "\x00")
   (if eicq-user-meta-hide-ip 
       "\x00" 
     "\x01")))

(defun eicq-pack-meta-user-update-work ()
  "Pack meta user packet 064a:03f2.
`M-x customize-group RET eicq-info' before this."
  (eicq-pack
   "\x4a\x06"
   "\xf2\x03"
   (eicq-int-bin (length eicq-user-meta-work-city))
   eicq-user-meta-work-city
   (eicq-int-bin (length eicq-user-meta-work-state))
   eicq-user-meta-work-state
   (eicq-int-bin (length eicq-user-meta-work-phone))
   eicq-user-meta-work-phone
   (eicq-int-bin (length eicq-user-meta-work-fax))
   eicq-user-meta-work-fax
   (eicq-int-bin (length eicq-user-meta-work-address))
   eicq-user-meta-work-address
   ;; unknown
   "\x00\x00\x00\x00\x00\x00"
   (eicq-int-bin (length eicq-user-meta-work-company))
   eicq-user-meta-work-company
   (eicq-int-bin (length eicq-user-meta-work-department))
   eicq-user-meta-work-department
   (eicq-int-bin (length eicq-user-meta-work-position))
   eicq-user-meta-work-position
   ;; unknown
   "\x00\x00"
   (eicq-int-bin (length eicq-user-meta-work-homepage))
   eicq-user-meta-work-homepage))

(defun eicq-pack-meta-user-update-more ()
  "Pack meta user packet 064a:03fc.
`M-x customize-group RET eicq-info' before this."
  (eicq-pack
   "\x4a\x06"
   "\xfc\x03"
   (eicq-int-bin eicq-user-meta-age)
   (case eicq-user-meta-sex
     (male "\x02")
     (female "\x01")
     (otherwise "\x00"))
   (eicq-int-bin (length eicq-user-meta-homepage))
   eicq-user-meta-homepage
   (eicq-int-byte eicq-user-meta-birth-year)
   (eicq-int-byte eicq-user-meta-birth-month)
   (eicq-int-byte eicq-user-meta-birth-day)
   (eicq-int-byte eicq-user-meta-language-1)
   (eicq-int-byte eicq-user-meta-language-2)
   (eicq-int-byte eicq-user-meta-language-3)))

(defun eicq-pack-meta-user-update-about ()
  "Pack meta user packet 064a:0406.
`M-x customize-group RET eicq-info' before this."
  (eicq-pack
   "\x4a\x06"
   "\x06\x04"
   (eicq-int-bin (length eicq-user-meta-about))
   eicq-user-meta-about))

(defvar eicq-do-meta-alist
  '(("\x64\x00" . eicq-do-meta-user-update-general-confirm)
    ("\x6e\x00" . eicq-do-meta-user-update-work-confirm)
    ("\x78\x00" . eicq-do-meta-user-update-more-confirm)
    ("\x82\x00" . eicq-do-meta-user-update-about-confirm)
    ("\xaa\x00" . eicq-do-meta-user-password)
    ("\xc8\x00" . eicq-do-meta-user-general)
    ("\xd2\x00" . eicq-do-meta-user-work)
    ("\xdc\x00" . eicq-do-meta-user-more)
    ("\xe6\x00" . eicq-do-meta-user-about)
    ("\xf0\x00" . eicq-do-meta-user-interest)
    ("\xfa\x00" . eicq-do-meta-user-background)
    ("\x0e\x01" . eicq-do-meta-user-picture)
    ("\x9a\x01" . eicq-do-meta-user-found))
  "Handlers for server packet meta user subcommands.")

(defun eicq-do-meta-user (packet)
  "Handle server command 03de in PACKET."
  (let* ((subcommand (substring packet 21 23))
         (result (substring packet 23 24))
         (data (substring packet 24))
         (handler (cdr (assoc subcommand eicq-do-meta-alist))))
    (if (not (equal result "\x0a"))
        (eicq-log-info "meta user command failed")
      (if (fboundp handler)
          (funcall handler data)
        (eicq-do-meta-user-unknown packet)))))

(defun eicq-do-meta-user-unknown (packet)
  "Handle server command 03de unknown subcommands in PACKET."
  (eicq-log-debug
   "meta user subcommand %s unhandled = %s"
   (eicq-bin-hex (substring packet 21 23))
   (eicq-bin-pretty-hex (substring packet 24))))

(defvar eicq-monthnames
  ["0" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])

(defun eicq-do-meta-user-general (data)
  "Handle server command 03de subcommand 00c8 in DATA."
  (let* ((i 0)
         (nickname-len (eicq-bin-int data i))
         (nickname (substring data (incf i 2) (1- (incf i nickname-len))))
         (first-name-len (eicq-bin-int data i))
         (first-name
          (substring data (incf i 2) (1- (incf i first-name-len))))
         (last-name-len (eicq-bin-int data i))
         (last-name
          (substring data (incf i 2) (1- (incf i last-name-len))))
         (primary-email-len (eicq-bin-int data i))
         (primary-email
          (substring data (incf i 2) (1- (incf i primary-email-len))))
         (secondary-email-len (eicq-bin-int data i))
         (secondary-email
          (substring data (incf i 2) (1- (incf i secondary-email-len))))
         (old-email-len (eicq-bin-int data i))
         (old-email
          (substring data (incf i 2) (1- (incf i old-email-len))))
         (city-len (eicq-bin-int data i))
         (city (substring data (incf i 2) (1- (incf i city-len))))
         (state-len (eicq-bin-int data i))
         (state (substring data (incf i 2) (1- (incf i state-len))))
         (phone-len (eicq-bin-int data i))
         (phone (substring data (incf i 2) (1- (incf i phone-len))))
         (fax-len (eicq-bin-int data i))
         (fax (substring data (incf i 2) (1- (incf i fax-len))))
         (street-len (eicq-bin-int data i))
         (street (substring data (incf i 2) (1- (incf i street-len))))
         (cellphone-len (eicq-bin-int data i))
         (cellphone
          (substring data (incf i 2) (1- (incf i cellphone-len))))
         (zipcode (substring data i (incf i 4)))
         (country (eicq-bin-int data i))
         (timezone (substring data (incf i 2) (incf i)))
         (authorization (eicq-byte-int data i))
         (web-aware (substring data (incf i) (incf i)))
         (hide-ip (substring data i (incf i))))
    (eicq-log-info
     (eicq-decode-string
      (format
       "meta user general info =
       Nickname: %s
      Firstname: %s
       Lastname: %s
  Primary email: %s
Secondary email: %s
      Old email: %s
           City: %s
          State: %s
          Phone: %s
            Fax: %s
         Street: %s
      Cellphone: %s
        Zipcode: %s
        Country: %s
       Timezone: %s
  Authorization: %s
      Web-aware: %s
        Hide-IP: %s"
       nickname first-name last-name primary-email secondary-email
       old-email city state phone fax street cellphone
       (eicq-bin-hex zipcode)
       (cdr (assoc country eicq-country-code))
       (eicq-bin-hex timezone)
       (if (= authorization 0) 
	   "Needed" 
	 "Not Needed")
       (eicq-bin-hex web-aware)
       (eicq-bin-hex hide-ip))))))

(defvar unknown)
(defvar unknown-2)

(defun eicq-do-meta-user-work (data)
  "Handle server command 03de subcommand 00d2 in DATA."
  (let* ((i 0)
         (city-len (eicq-bin-int data i))
         (city (substring data (incf i 2) (1- (incf i city-len))))
         (state-len (eicq-bin-int data i))
         (state (substring data (incf i 2) (1- (incf i state-len))))
         (phone-len (eicq-bin-int data i))
         (phone (substring data (incf i 2) (1- (incf i phone-len))))
         (fax-len (eicq-bin-int data i))
         (fax (substring data (incf i 2) (1- (incf i fax-len))))
         (address-len (eicq-bin-int data i))
         (address (substring data (incf i 2) (1- (incf i address-len))))
         (unknown (incf i 6))
         (company-len (eicq-bin-int data i))
         (company (substring data (incf i 2) (1- (incf i company-len))))
         (department-len (eicq-bin-int data i))
         (department (substring data (incf i 2) (1- (incf i department-len))))
         (position-len (eicq-bin-int data i))
         (position (substring data (incf i 2) (1- (incf i position-len))))
         (unknown-2 (incf i 2))
         (homepage-len (eicq-bin-int data i))
         (homepage
          (substring data (incf i 2) (1- (incf i homepage-len)))))
    ;; ignore empty data
    (when (> (length data) 35)
      (eicq-log-info
       (eicq-decode-string
        (format
         "meta user work info =
      City: %s
     State: %s
     Phone: %s
       Fax: %s
   Address: %s
   Company: %s
Department: %s
  Position: %s
  Homepage: %s"
         city state phone fax address company department position homepage))))))

(defun eicq-do-meta-user-more (data)
  "Handle server command 03de subcommand 00dc in DATA."
  (let* ((i 0)
         (age (eicq-bin-int data i))
         (sex (substring data (incf i 2) (incf i)))
         (homepage-len (eicq-bin-int data i))
         (homepage
          (substring data (incf i 2) (1- (incf i homepage-len))))
         (birth-year (eicq-byte-int data i))
         (birth-month (eicq-byte-int data (incf i)))
         (birth-day (eicq-byte-int data (incf i)))
         (language-1 (eicq-byte-int data (incf i)))
         (language-2 (eicq-byte-int data (incf i)))
         (language-3 (eicq-byte-int data (incf i))))
    ;; ignore empty data
    (when (> (length data) 12)
      (eicq-log-info
       (eicq-decode-string
        (format
         "meta user more info =
       Age: %s
       Sex: %s
  Homepage: %s
  Birthday: %s %s, 19%02s
Language-1: %s
Language-2: %s
Language-3: %s"
         (if (= age 65535) "not entered" age)
         (cond
          ((string= sex "\x00") "not entered")
          ((string= sex "\x01") "female")
          ((string= sex "\x02") "male"))
         homepage
         (aref eicq-monthnames birth-month)
         birth-day birth-year
         language-1 language-2 language-3))))))

(defun eicq-do-meta-user-about (data)
  "Handle server command 03de subcommand 00e6 in DATA."
  ;; ignore empty data
  (when (> (eicq-bin-int data) 1)
    (eicq-log-info
     "meta user about info =\n%s"
     (eicq-decode-string (substring data 2 -1)))))

(defun eicq-do-meta-user-interest (data)
  "Handle server command 03de subcommand 00f0 in DATA."
  ;; ignore empty data
  (when (> (length data) 1)
    (eicq-log-info "meta user interest info =\n%s"
                   (eicq-decode-string data))))

(defun eicq-do-meta-user-background (data)
  "Handle server command 03de subcommand 00fa in DATA."
  ;; ignore empty data
  (when (> (eicq-bin-int data) 1)
    (eicq-log-info "meta user background info =\n%s"
                   (eicq-decode-string data))))

(defun eicq-do-meta-user-picture (data)
  "Handle server command 03de subcommand 00fa in DATA."
  ;; ignore empty data
  (when (> (length data) 1)
    (eicq-log-info "meta user picture info =\n%s"
                   (eicq-decode-string data))))

(defun eicq-do-meta-user-found (data)
  "Handle server command 03de subcommand 019a in DATA."
  (eicq-log-info "meta user found\n%s"
                 (eicq-bin-pretty-hex data)))

(defun eicq-do-meta-user-update-general-confirm (data)
  "Handle server command 03de subcommand 0064 in DATA."
  (eicq-log-info "meta user update general info succeeded"))

(defun eicq-do-meta-user-update-work-confirm (data)
  "Handle server command 03de subcommand 006e in DATA."
  (eicq-log-info "meta user update work info succeeded"))

(defun eicq-do-meta-user-update-more-confirm (data)
  "Handle server command 03de subcommand 0078 in DATA."
  (eicq-log-info "meta user update more info succeeded"))

(defun eicq-do-meta-user-update-about-confirm (data)
  "Handle server command 03de subcommand 0082 in DATA."
  (eicq-log-info "meta user update about info succeeded"))

(defun eicq-do-meta-user-update-security-confirm (data)
  "Handle server command 03de subcommand a000 in DATA."
  (eicq-log-info "meta user update security info succeeded"))

(defun eicq-do-meta-user-password (data)
  "Handle server command 03de subcommand 00aa in DATA."
  (eicq-log-info "meta user password change succeeded"))

(defun eicq-update-meta-info ()
  "Update user meta info on the ICQ server.
Run this after changing any meta user info variables."
  (interactive)
  ;(eicq-send (eicq-pack-update-authorization))
  (eicq-send (eicq-pack-meta-user-security))
  (eicq-send (eicq-pack-meta-user-update-general))
  (eicq-send (eicq-pack-meta-user-update-work))
  (eicq-send (eicq-pack-meta-user-update-more))
  (eicq-send (eicq-pack-meta-user-update-about)))

(provide 'eicq-meta)

;;; eicq-status.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End: 
