module Main exposing (main)

{-

   Copyright 2018 Fabian Kirchner

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

-}

import Browser
import DropdownMenu.Filterable
import DropdownMenu.Simple
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Lazy as Html


main : Program {} Model Msg
main =
    Browser.embed
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { selectedLicense : Maybe String
    , licenseMenu : DropdownMenu.Filterable.State String
    , selectedLocale : Maybe String
    , localeMenu : DropdownMenu.Simple.State String
    , selectedNumber : Maybe String
    , numberMenu : DropdownMenu.Filterable.State String
    }


init _ =
    ( { selectedLicense = Nothing
      , licenseMenu = DropdownMenu.Filterable.closed
      , selectedLocale = Nothing
      , localeMenu = DropdownMenu.Simple.closed
      , selectedNumber = Nothing
      , numberMenu = DropdownMenu.Filterable.closed
      }
    , Cmd.none
    )


type Msg
    = LicenseMenuMsg (DropdownMenu.Filterable.Msg String)
    | LocaleMenuMsg (DropdownMenu.Simple.Msg String)
    | NumberMenuMsg (DropdownMenu.Filterable.Msg String)


type OutMsg
    = EntrySelected String
    | SelectionDismissed


update msg model =
    case msg of
        LicenseMenuMsg dropdownMenuMsg ->
            let
                ( newDropdownMenu, cmd, maybeOutMsg ) =
                    DropdownMenu.Filterable.update
                        { entrySelected = EntrySelected
                        , selectionDismissed = SelectionDismissed
                        }
                        model.selectedLicense
                        model.licenseMenu
                        dropdownMenuMsg

                newModel =
                    { model | licenseMenu = newDropdownMenu }
            in
            ( case maybeOutMsg of
                Just (EntrySelected entry) ->
                    { newModel | selectedLicense = Just entry }

                Just SelectionDismissed ->
                    { newModel | selectedLicense = Nothing }

                Nothing ->
                    newModel
            , Cmd.map LicenseMenuMsg cmd
            )

        LocaleMenuMsg dropdownMenuMsg ->
            let
                ( newLocaleMenu, cmd, outMsg ) =
                    DropdownMenu.Simple.update EntrySelected
                        model.localeMenu
                        dropdownMenuMsg

                newModel =
                    { model
                        | localeMenu = newLocaleMenu
                    }
            in
            ( case outMsg of
                Just (EntrySelected newSelection) ->
                    { newModel | selectedLocale = Just newSelection }

                _ ->
                    newModel
            , Cmd.map LocaleMenuMsg cmd
            )

        NumberMenuMsg dropdownMenuMsg ->
            let
                ( newNumberMenu, cmd, maybeOutMsg ) =
                    DropdownMenu.Filterable.update
                        { entrySelected = EntrySelected
                        , selectionDismissed = SelectionDismissed
                        }
                        model.selectedNumber
                        model.numberMenu
                        dropdownMenuMsg

                newModel =
                    { model | numberMenu = newNumberMenu }
            in
            ( case maybeOutMsg of
                Just (EntrySelected entry) ->
                    { newModel | selectedNumber = Just entry }

                Just SelectionDismissed ->
                    { newModel | selectedNumber = Nothing }

                Nothing ->
                    newModel
            , Cmd.map NumberMenuMsg cmd
            )


subscriptions _ =
    Sub.none


view model =
    Html.div
        [ Attributes.style "display" "flex"
        , Attributes.style "flex-flow" "column"
        ]
        [ Html.lazy2 viewLicenses model.selectedLicense model.licenseMenu
        , Html.lazy2 viewLocales model.selectedLocale model.localeMenu
        , Html.lazy2 viewNumbers model.selectedNumber model.numberMenu
        ]


viewLicenses selectedLicense licenseMenu =
    Html.div []
        [ Html.span
            [ Attributes.id "license__label" ]
            [ Html.text "License" ]
        , licenses
            |> DropdownMenu.Filterable.view filterableConfig
                { id = "license"
                , labelledBy = "license__label"
                , placeholder = "Select a license..."
                }
                selectedLicense
                licenseMenu
            |> Html.map LicenseMenuMsg
        ]


viewLocales selectedLocale localeMenu =
    Html.div []
        [ Html.span
            [ Attributes.id "locale__label" ]
            [ Html.text "Locale" ]
        , selectedLocale
            |> DropdownMenu.Simple.viewLazy (\_ -> 42)
                optionalConfig
                { id = "locale"
                , labelledBy = "locale__label"
                }
                localeMenu
                locales
            |> Html.map LocaleMenuMsg
        ]


viewNumbers selectedNumber numberMenu =
    Html.div []
        [ Html.span
            [ Attributes.id "number__label" ]
            [ Html.text "Number" ]
        , numbers
            |> DropdownMenu.Filterable.viewLazy (\_ -> 42)
                filterableConfig
                { id = "number"
                , labelledBy = "number__label"
                , placeholder = "Select a number..."
                }
                selectedNumber
                numberMenu
            |> Html.map NumberMenuMsg
        ]


filterableConfig : DropdownMenu.Filterable.Config String
filterableConfig =
    DropdownMenu.Filterable.config
        { matchesQuery =
            \query value ->
                String.toLower value
                    |> String.contains (String.toLower query)
        , entryId = identity
        , printEntry = identity
        , jumpAtEnds = True
        , closeAfterMouseSelection = False
        , container = [ Attributes.style "width" "500px" ]
        , textfield =
            \{ selection, open } -> [ Attributes.class "textfield" ]
        , ul = [ Attributes.class "list" ]
        , li =
            \{ selected, keyboardFocused, mouseFocused } name ->
                { attributes =
                    [ Attributes.class "entry"
                    , Attributes.classList
                        [ ( "entry--keyboard-focused", keyboardFocused )
                        , ( "entry--mouse-focused", mouseFocused )
                        ]
                    ]
                , children = [ Html.text name ]
                }
        }


optionalConfig : DropdownMenu.Simple.Config String
optionalConfig =
    DropdownMenu.Simple.config
        { matchesQuery =
            \query value ->
                String.toLower value
                    |> String.contains (String.toLower query)
        , entryId = identity
        , jumpAtEnds = True
        , closeAfterMouseSelection = False
        , container = [ Attributes.style "width" "500px" ]
        , button =
            \{ selection, open } ->
                { attributes = [ Attributes.class "button" ]
                , children =
                    [ selection
                        |> Maybe.withDefault "Select a locale..."
                        |> Html.text
                    ]
                }
        , ul = [ Attributes.class "list" ]
        , li =
            \{ selected, keyboardFocused, mouseFocused } name ->
                { attributes =
                    [ Attributes.class "entry"
                    , Attributes.classList
                        [ ( "entry--keyboard-focused", keyboardFocused )
                        , ( "entry--mouse-focused", mouseFocused )
                        ]
                    ]
                , children = [ Html.text name ]
                }
        }



---- DATA


numbers : List String
numbers =
    List.range 0 123456
        |> List.map String.fromInt


licenses : List String
licenses =
    List.concat
        [ gplCompatible
        , gplIncompatible
        , nonfree
        ]


gplCompatible : List String
gplCompatible =
    [ "GNU General Public License (GPL) version 3 (#GNUGPL) (#GNUGPLv3)"
    , "GNU General Public License (GPL) version 2 (#GPLv2)"
    , "GNU Lesser General Public License (LGPL) version 3 (#LGPL) (#LGPLv3)"
    , "GNU Lesser General Public License (LGPL) version 2.1 (#LGPLv2.1)"
    , "GNU Affero General Public License (AGPL) version 3 (#AGPL) (#AGPLv3.0)"
    , "GNU All-Permissive License (#GNUAllPermissive)"
    , "Apache License, Version 2.0 (#apache2)"
    , "Artistic License 2.0 (#ArtisticLicense2)"
    , "Clarified Artistic License"
    , "Berkeley Database License (a.k.a. the Sleepycat Software Product License) (#BerkeleyDB)"
    , "Boost Software License (#boost)"
    , "Modified BSD license (#ModifiedBSD)"
    , "CC0 (#CC0)"
    , "CeCILL version 2 (#CeCILL)"
    , "The Clear BSD License (#clearbsd)"
    , "Cryptix General License (#CryptixGeneralLicense)"
    , "eCos license version 2.0 (#eCos2.0)"
    , "Educational Community License 2.0 (#ECL2.0)"
    , "Eiffel Forum License, version 2 (#Eiffel)"
    , "EU DataGrid Software License (#EUDataGrid)"
    , "Expat License (#Expat)"
    , "FreeBSD license (#FreeBSD)"
    , "Freetype Project License (#freetype)"
    , "Historical Permission Notice and Disclaimer (#HPND)"
    , "License of the iMatix Standard Function Library (#iMatix)"
    , "License of imlib2 (#imlib)"
    , "Independent JPEG Group License (#ijg)"
    , "Informal license (#informal)"
    , "Intel Open Source License (#intel)"
    , "ISC License (#ISC)"
    , "Mozilla Public License (MPL) version 2.0 (#MPL-2.0)"
    , "NCSA/University of Illinois Open Source License (#NCSA)"
    , "License of Netscape JavaScript (#NetscapeJavaScript)"
    , "OpenLDAP License, Version 2.7 (#newOpenLDAP)"
    , "License of Perl 5 and below (#PerlLicense)"
    , "Public Domain (#PublicDomain)"
    , "License of Python 2.0.1, 2.1.1, and newer versions (#Python)"
    , "License of Python 1.6a2 and earlier versions (#Python1.6a2)"
    , "License of Ruby (#Ruby)"
    , "SGI Free Software License B, version 2.0 (#SGIFreeB)"
    , "Standard ML of New Jersey Copyright License (#StandardMLofNJ)"
    , "Unicode, Inc. License Agreement for Data Files and Software (#Unicode)"
    , "Universal Permissive License (UPL) (#UPL)"
    , "The Unlicense (#Unlicense)"
    , "License of Vim, Version 6.1 or later (#Vim)"
    , "W3C Software Notice and License (#W3C)"
    , "License of WebM (#WebM)"
    , "WTFPL, Version 2 (#WTFPL)"
    , "WxWidgets License (#Wx)"
    , "X11 License (#X11License)"
    , "XFree86 1.1 License (#XFree861.1License)"
    , "License of ZLib (#ZLib)"
    , "Zope Public License, versions 2.0 and 2.1 (#Zope2.0)"
    ]


gplIncompatible : List String
gplIncompatible =
    [ "Affero General Public License version 1 (#AGPLv1.0)"
    , "Academic Free License, all versions through 3.0 (#AcademicFreeLicense)"
    , "Apache License, Version 1.1 (#apache1.1)"
    , "Apache License, Version 1.0 (#apache1)"
    , "Apple Public Source License (APSL), version 2 (#apsl2)"
    , "BitTorrent Open Source License (#bittorrent)"
    , "Original BSD license (#OriginalBSD)"
    , "Common Development and Distribution License (CDDL), version 1.0 (#CDDL)"
    , "Common Public Attribution License 1.0 (CPAL) (#CPAL)"
    , "Common Public License Version 1.0 (#CommonPublicLicense10)"
    , "Condor Public License (#Condor)"
    , "Eclipse Public License Version 1.0 (#EPL)"
    , "European Union Public License (EUPL) version 1.1 (#EUPL)"
    , "Gnuplot license (#gnuplot)"
    , "IBM Public License, Version 1.0 (#IBMPL)"
    , "Jabber Open Source License, Version 1.0 (#josl)"
    , "LaTeX Project Public License 1.3a (#LPPL-1.3a)"
    , "LaTeX Project Public License 1.2 (#LPPL-1.2)"
    , "Lucent Public License Version 1.02 (Plan 9 license) (#lucent102)"
    , "Microsoft Public License (Ms-PL) (#ms-pl)"
    , "Microsoft Reciprocal License (Ms-RL) (#ms-rl)"
    , "Mozilla Public License (MPL) version 1.1 (#MPL)"
    , "Netizen Open Source License (NOSL), Version 1.0 (#NOSL)"
    , "Netscape Public License (NPL), versions 1.0 and 1.1 (#NPL)"
    , "Nokia Open Source License (#Nokia)"
    , "Old OpenLDAP License, Version 2.3 (#oldOpenLDAP)"
    , "Open Software License, all versions through 3.0 (#OSL)"
    , "OpenSSL license (#OpenSSL)"
    , "Phorum License, Version 2.0 (#Phorum)"
    , "PHP License, Version 3.01 (#PHP-3.01)"
    , "License of Python 1.6b1 through 2.0 and 2.1 (#PythonOld)"
    , "Q Public License (QPL), Version 1.0 (#QPL)"
    , "RealNetworks Public Source License (RPSL), Version 1.0 (#RPSL)"
    , "Sun Industry Standards Source License 1.0 (#SISSL)"
    , "Sun Public License (#SPL)"
    , "License of xinetd (#xinetd)"
    , "Yahoo! Public License 1.1 (#Yahoo)"
    , "Zend License, Version 2.0 (#Zend)"
    , "Zimbra Public License 1.3 (#Zimbra)"
    , "Zope Public License version 1 (#Zope)"
    ]


nonfree : List String
nonfree =
    [ "No license (#NoLicense)"
    , "Aladdin Free Public License (#Aladdin)"
    , "Apple Public Source License (APSL), version 1.x (#apsl1)"
    , "Artistic License 1.0 (#ArtisticLicense)"
    , "AT&T Public License (#ATTPublicLicense)"
    , "eCos Public License, version 1.1 (#eCos11)"
    , "CNRI Digital Object Repository License Agreement (#DOR)"
    , "GPL for Computer Programs of the Public Administration (#GPL-PA)"
    , "Jahia Community Source License (#Jahia)"
    , "The JSON License (#JSON)"
    , "Old license of ksh93 (#ksh93)"
    , "License of Lha (#Lha)"
    , "Microsoft's Shared Source CLI, C#, and Jscript License (#Ms-SS)"
    , "NASA Open Source Agreement (#NASA)"
    , "Oculus Rift SDK License (#OculusRiftSDK)"
    , "Peer-Production License (#PPL)"
    , "License of PINE (#PINE)"
    , "Old Plan 9 license (#Plan9)"
    , "Reciprocal Public License (#RPL)"
    , "Scilab license (#Scilab)"
    , "Scratch 1.4 license (#Scratch)"
    , "Simple Machines License (#SML)"
    , "Sun Community Source License (#SunCommunitySourceLicense)"
    , "Sun Solaris Source Code (Foundation Release) License, Version 1.1 (#SunSolarisSourceCode)"
    , "Sybase Open Watcom Public License version 1.0 (#Watcom)"
    , "SystemC “Open Source” License, Version 3.0 (#SystemC-3.0)"
    , "Truecrypt license 3.0 (#Truecrypt-3.0)"
    , "University of Utah Public License (#UtahPublicLicense)"
    , "YaST License (#YaST)"
    ]


locales : List String
locales =
    [ "Abkhazian"
    , "Achinese"
    , "Acoli"
    , "Adangme"
    , "Adyghe"
    , "Afar"
    , "Afar, Djibouti"
    , "Afar, Eritrea"
    , "Afar, Ethiopia"
    , "Afar(Ethiopic)"
    , "Afrihili"
    , "Afrikaans"
    , "Afrikaans, Namibia"
    , "Afrikaans, South Africa"
    , "Afro-Asiatic Language"
    , "Ainu"
    , "Ainu(Latin)"
    , "Akan"
    , "Akan, Ghana"
    , "Akkadian"
    , "Albanian"
    , "Albanian, Albania"
    , "Aleut"
    , "Algonquian Language"
    , "Altaic Language"
    , "Amharic"
    , "Amharic, Ethiopia"
    , "Angika"
    , "Apache Language"
    , "Arabic"
    , "Arabic, Algeria"
    , "Arabic(Perso-Arabic)"
    , "Arabic, Bahrain"
    , "Arabic, Egypt"
    , "Arabic, Iraq"
    , "Arabic, Jordan"
    , "Arabic, Kuwait"
    , "Arabic, Lebanon"
    , "Arabic, Libya"
    , "Arabic, Morocco"
    , "Arabic, Oman"
    , "Arabic, Qatar"
    , "Arabic, Saudi Arabia"
    , "Arabic, Sudan"
    , "Arabic, Syria"
    , "Arabic, Tunisia"
    , "Arabic, United Arab Emirates"
    , "Arabic, Yemen"
    , "Aragonese"
    , "Arapaho"
    , "Araucanian"
    , "Arawak"
    , "Armenian"
    , "Armenian, Armenia"
    , "Aromanian"
    , "Aromanian(Greek)"
    , "Aromanian(Latin)"
    , "Artificial Language"
    , "Assamese"
    , "Assamese, India"
    , "Asturian"
    , "Athapascan Language"
    , "Atsam"
    , "Atsam, Nigeria"
    , "Australian Language"
    , "Austronesian Language"
    , "Avaric"
    , "Avestan"
    , "Awadhi"
    , "Aymara"
    , "Azeri"
    , "Azeri(Perso-Arabic)"
    , "Azeri, Azerbaijan"
    , "Azeri, Azerbaijan(Cyrillic)"
    , "Azeri, Azerbaijan(Latin)"
    , "Azeri(Cyrillic)"
    , "Azeri(Latin)"
    , "Azeri, Iran"
    , "Azeri, Iran(Perso-Arabic)"
    , "Balinese"
    , "Baltic Language"
    , "Baluchi"
    , "Baluchi(Perso-Arabic)"
    , "Bambara"
    , "Bamileke Language"
    , "Banda"
    , "Bantu"
    , "Basa"
    , "Bashkir"
    , "Basque"
    , "Basque, France"
    , "Basque, Spain"
    , "Batak"
    , "Beja"
    , "Belarusian"
    , "Belarusian, Belarus"
    , "Belarusian(Cyrillic)"
    , "Belarusian(Latin)"
    , "Bemba"
    , "bengali"
    , "bengali, bangladesh"
    , "Bengali, India"
    , "Berber"
    , "Bhojpuri"
    , "Bihari"
    , "Bikol"
    , "Bini"
    , "Bislama"
    , "Blin"
    , "Blin, Eritrea"
    , "Blissymbols"
    , "Bosnian"
    , "Bosnian, Bosnia and Herzegovina"
    , "Braj"
    , "Breton"
    , "Buginese"
    , "Bulgarian"
    , "Bulgarian, Bulgaria"
    , "Buriat"
    , "Burmese"
    , "Burmese, Myanmar [Burma]"
    , "Caddo"
    , "Carib"
    , "Catalan"
    , "Catalan, Spain"
    , "Caucasian Language"
    , "Cebuano"
    , "Celtic Language"
    , "Central American Indian Language"
    , "Khmer"
    , "Khmer, Cambodia"
    , "Chamic Language"
    , "Chamic Language(Perso-Arabic)"
    , "Chamorro"
    , "Chechen"
    , "Cherokee"
    , "Cheyenne"
    , "Nyanja"
    , "Nyanja, Malawi"
    , "Chinese"
    , "Chinese, China"
    , "Chinese, China(Simplified Han)"
    , "Chinese, Hong Kong"
    , "Chinese, Hong Kong(Simplified Han)"
    , "Chinese, Hong Kong(Traditional Han)"
    , "Chinese, Macau"
    , "Chinese, Macau(Simplified Han)"
    , "Chinese, Macau(Traditional Han)"
    , "Chinese(Simplified Han)"
    , "Chinese, Singapore"
    , "Chinese, Singapore(Simplified Han)"
    , "Chinese, Taiwan"
    , "Chinese, Taiwan(Traditional Han)"
    , "Chinese(Traditional Han)"
    , "Chinook Jargon"
    , "Chipewyan"
    , "Choctaw"
    , "Church Slavic"
    , "Chuukese"
    , "Chuvash"
    , "Cornish"
    , "Cornish, United Kingdom"
    , "Corsican"
    , "Cree"
    , "Creek"
    , "Creole or Pidgin"
    , "Crimean Turkish"
    , "Crimean Turkish(Cyrillic)"
    , "Crimean Turkish(Latin)"
    , "Croatian"
    , "Croatian, Croatia"
    , "Cushitic Language"
    , "Czech"
    , "Czech, Czech Republic"
    , "Dakota"
    , "Danish"
    , "Danish, Denmark"
    , "Dargwa"
    , "Dayak"
    , "Delaware"
    , "Dinka"
    , "Divehi"
    , "Divehi, Maldives"
    , "Divehi(Thaana)"
    , "Dogri"
    , "Dogrib"
    , "Dravidian Language"
    , "Duala"
    , "Dutch"
    , "Dutch, Belgium"
    , "Dutch, Netherlands"
    , "Dyula"
    , "Dzongkha"
    , "Dzongkha, Bhutan"
    , "Eastern Frisian"
    , "Efik"
    , "Ekajuk"
    , "English"
    , "English, American Samoa"
    , "English, Australia"
    , "English-based Creole or Pidgin"
    , "English, Belgium"
    , "English, Belize"
    , "English, Botswana"
    , "English, Canada"
    , "English(Deseret)"
    , "English, Guam"
    , "English, Hong Kong"
    , "English, India"
    , "English, Ireland"
    , "English, Israel"
    , "English, Jamaica"
    , "English, Malta"
    , "English, Marshall Islands"
    , "English, Namibia"
    , "English, New Zealand"
    , "English, Northern Mariana Islands"
    , "English, Pakistan"
    , "English, Philippines"
    , "English(Shavian)"
    , "English, Singapore"
    , "English, South Africa"
    , "English, Trinidad and Tobago"
    , "English, United Kingdom"
    , "English, United States"
    , "English, United States(Deseret)"
    , "English, U.S. Minor Outlying Islands"
    , "English, U.S. Virgin Islands"
    , "English, Zimbabwe"
    , "Erzya"
    , "Esperanto"
    , "Estonian"
    , "Estonian, Estonia"
    , "Ewe"
    , "Ewe, Ghana"
    , "Ewe, Togo"
    , "Ewondo"
    , "Fang"
    , "Fanti"
    , "Faroese"
    , "Faroese, Faroe Islands"
    , "Fijian"
    , "Filipino"
    , "Filipino, Philippines"
    , "Finnish"
    , "Finnish, Finland"
    , "Finno-Ugrian Language"
    , "Fon"
    , "French"
    , "French-based Creole or Pidgin"
    , "French, Belgium"
    , "French, Canada"
    , "French, France"
    , "French, Luxembourg"
    , "French, Monaco"
    , "French, Senegal"
    , "French, Switzerland"
    , "French, Morocco"
    , "Friulian"
    , "Friulian, Italy"
    , "Fulah"
    , "Fulah(Perso-Arabic)"
    , "Fulah(Latin)"
    , "Ga"
    , "Scottish Gaelic"
    , "Ga, Ghana"
    , "Galician"
    , "Galician, Spain"
    , "Ganda"
    , "Gayo"
    , "Gbaya"
    , "Geez"
    , "Geez, Eritrea"
    , "Geez, Ethiopia"
    , "Georgian"
    , "Georgian, Georgia"
    , "German"
    , "German, Austria"
    , "German, Belgium"
    , "German, Germany"
    , "Germanic Language"
    , "German, Liechtenstein"
    , "German, Luxembourg"
    , "German, Switzerland"
    , "Gilbertese"
    , "Gondi"
    , "Gorontalo"
    , "Grebo"
    , "Greek"
    , "Greek, Cyprus"
    , "Greek, Greece"
    , "Guarani"
    , "Gujarati"
    , "Gujarati, India"
    , "Gwichʼin"
    , "Haida"
    , "Haitian"
    , "Hausa"
    , "Hausa(Perso-Arabic)"
    , "Hausa, Ghana"
    , "Hausa, Ghana(Latin)"
    , "Hausa(Latin)"
    , "Hausa, Niger"
    , "Hausa, Nigeria"
    , "Hausa, Nigeria(Perso-Arabic)"
    , "Hausa, Nigeria(Latin)"
    , "Hausa, Niger(Latin)"
    , "Hausa, Sudan"
    , "Hausa, Sudan(Perso-Arabic)"
    , "Hawaiian"
    , "Hawaiian, United States"
    , "Hebrew"
    , "Hebrew(Hebrew)"
    , "Hebrew, Israel"
    , "Herero"
    , "Hiligaynon"
    , "Himachali"
    , "Hindi"
    , "Hindi, India"
    , "Hiri Motu"
    , "Hittite"
    , "Hmong"
    , "Hungarian"
    , "Hungarian, Hungary"
    , "Hupa"
    , "Iban"
    , "Icelandic"
    , "Icelandic, Iceland"
    , "Ido"
    , "Igbo"
    , "Igbo, Nigeria"
    , "Ijo"
    , "Iloko"
    , "Inari Sami"
    , "Indic Language"
    , "Indo-European Language"
    , "Indonesian"
    , "Indonesian(Perso-Arabic)"
    , "Indonesian, Indonesia"
    , "Indonesian, Indonesia(Perso-Arabic)"
    , "Ingush"
    , "Interlingua"
    , "Interlingue"
    , "Inuktitut"
    , "Inuktitut, Canada"
    , "Inupiaq"
    , "Iranian Language"
    , "Irish"
    , "Irish, Ireland"
    , "Iroquoian Language"
    , "Italian"
    , "Italian, Italy"
    , "Italian, Switzerland"
    , "Japanese"
    , "Japanese, Japan"
    , "Javanese"
    , "Javanese(Javanese)"
    , "Javanese(Latin)"
    , "Judeo-Arabic"
    , "Judeo-Persian"
    , "Kabardian"
    , "Kabyle"
    , "Kachin"
    , "Kalaallisut"
    , "Kalaallisut, Greenland"
    , "Kalmyk"
    , "Kalmyk(Cyrillic)"
    , "Kalmyk(Mongolian)"
    , "Kamba"
    , "Kamba, Kenya"
    , "Kannada"
    , "Kannada, India"
    , "Kanuri"
    , "Karachay-Balkar"
    , "Kara-Kalpak"
    , "Karelian"
    , "Karen"
    , "Kashmiri"
    , "Kashmiri(Perso-Arabic)"
    , "Kashmiri(Devanagari)"
    , "Kashmiri(Latin)"
    , "Kashubian"
    , "Kawi"
    , "Kazakh"
    , "Kazakh(Perso-Arabic)"
    , "Kazakh(Cyrillic)"
    , "Kazakh, Kazakhstan"
    , "Kazakh, Kazakhstan(Perso-Arabic)"
    , "Kazakh, Kazakhstan(Cyrillic)"
    , "Kazakh, Kazakhstan(Latin)"
    , "Kazakh(Latin)"
    , "Khasi"
    , "Khoisan Language"
    , "Khotanese"
    , "Kikuyu"
    , "Kimbundu"
    , "Kinyarwanda"
    , "Kinyarwanda, Rwanda"
    , "Kirghiz(Cyrillic)"
    , "Kirghiz"
    , "Kirghiz(Perso-Arabic)"
    , "Kirghiz, Kyrgyzstan"
    , "Kirghiz(Latin)"
    , "Klingon"
    , "Komi"
    , "Kongo"
    , "Konkani"
    , "Konkani, India"
    , "Konkani, India(Kannada)"
    , "Konkani, India(Latin)"
    , "Konkani, India(Malayalam)"
    , "Konkani(Kannada)"
    , "Konkani(Latin)"
    , "Konkani(Malayalam)"
    , "Korean"
    , "Korean, South Korea"
    , "Koro"
    , "Koro, Ivory Coast"
    , "Kosraean"
    , "Kpelle"
    , "Kpelle, Guinea"
    , "Kpelle, Liberia"
    , "Kru"
    , "Kuanyama"
    , "Kumyk"
    , "Kurdish"
    , "Kurdish(Perso-Arabic)"
    , "Kurdish, Iran"
    , "Kurdish, Iran(Perso-Arabic)"
    , "Kurdish, Iraq"
    , "Kurdish, Iraq(Perso-Arabic)"
    , "Kurdish(Latin)"
    , "Kurdish, Syria"
    , "Kurdish, Syria(Perso-Arabic)"
    , "Kurdish, Turkey"
    , "Kurdish, Turkey(Latin)"
    , "Kurukh"
    , "Kutenai"
    , "Ladino"
    , "Ladino(Hebrew)"
    , "Ladino(Latin)"
    , "Lahnda"
    , "Lamba"
    , "Lao"
    , "Lao, Laos"
    , "Latin"
    , "Latvian"
    , "Latvian, Latvia"
    , "Lezghian"
    , "Limburgish"
    , "Lingala"
    , "Lingala, Congo [Republic]"
    , "Lingala, Congo [DRC]"
    , "Lithuanian"
    , "Lithuanian, Lithuania"
    , "Lojban"
    , "Lower Sorbian"
    , "Low German"
    , "Low German, Germany"
    , "Lozi"
    , "Luba-Katanga"
    , "Luba-Lulua"
    , "Luiseno"
    , "Lule Sami"
    , "Lunda"
    , "Luo"
    , "Lushai"
    , "Luxembourgish"
    , "Macedonian"
    , "Macedonian, Macedonia [FYROM]"
    , "Madurese"
    , "Magahi"
    , "Maithili"
    , "Makasar"
    , "Makasar(Buginese)"
    , "Makasar(Latin)"
    , "Malagasy"
    , "Malay"
    , "Malayalam"
    , "Malayalam(Perso-Arabic)"
    , "Malayalam, India"
    , "Malayalam, India(Perso-Arabic)"
    , "Malayalam, India(Malayalam)"
    , "Malayalam(Malayalam)"
    , "Malay(Perso-Arabic)"
    , "Malay, Brunei"
    , "Malay, Brunei(Latin)"
    , "Malay(Latin)"
    , "Malay, Malaysia"
    , "Malay, Malaysia(Latin)"
    , "Maltese"
    , "Maltese, Malta"
    , "Manchu"
    , "Mandar"
    , "Mandingo"
    , "Manipuri"
    , "Manobo Language"
    , "Manx"
    , "Manx, United Kingdom"
    , "Maori"
    , "Marathi"
    , "Marathi, India"
    , "Mari"
    , "Marshallese"
    , "Marwari"
    , "Masai"
    , "Mayan Language"
    , "Mende"
    , "Micmac"
    , "Minangkabau"
    , "Mirandese"
    , "Mohawk"
    , "Moksha"
    , "Moldavian"
    , "Moldavian, Moldova"
    , "Mongo"
    , "Mongolian"
    , "Mongolian, China"
    , "Mongolian, China(Mongolian)"
    , "Mongolian(Cyrillic)"
    , "Mongolian, Mongolia"
    , "Mongolian, Mongolia(Cyrillic)"
    , "Mongolian(Mongolian)"
    , "Mon-Khmer Language"
    , "Mossi"
    , "Multiple Languages"
    , "Munda Language"
    , "Nahuatl"
    , "Nauru"
    , "Navajo"
    , "North Ndebele"
    , "South Ndebele"
    , "South Ndebele, South Africa"
    , "Ndonga"
    , "Neapolitan"
    , "Newari"
    , "Nepali"
    , "Nepali, India"
    , "Nepali, Nepal"
    , "Nias"
    , "Niger-Kordofanian Language"
    , "Nilo-Saharan Language"
    , "Niuean"
    , "N’Ko"
    , "Nogai"
    , "No linguistic content"
    , "North American Indian Language"
    , "Northern Frisian"
    , "Northern Sami"
    , "Northern Sami, Finland"
    , "Northern Sami, Norway"
    , "Norwegian Bokmål"
    , "Norwegian Bokmål, Norway"
    , "Norwegian Nynorsk"
    , "Norwegian Nynorsk, Norway"
    , "Nubian Language"
    , "Nyamwezi"
    , "Nyankole"
    , "Nyoro"
    , "Nzima"
    , "Occitan"
    , "Occitan, France"
    , "Ojibwa"
    , "Oriya"
    , "Oriya, India"
    , "Oromo"
    , "Oromo, Ethiopia"
    , "Oromo, Kenya"
    , "Osage"
    , "Ossetic"
    , "Ossetic(Cyrillic)"
    , "Ossetic(Latin)"
    , "Otomian Language"
    , "Pahlavi"
    , "Palauan"
    , "Pali"
    , "Pali(Devanagari)"
    , "Pali(Sinhala)"
    , "Pali(Thai)"
    , "Pampanga"
    , "Pampanga, India"
    , "Pangasinan"
    , "Punjabi(Perso-Arabic)"
    , "Punjabi(Devanagari)"
    , "Punjabi(Gurmukhi)"
    , "Punjabi, India(Devanagari)"
    , "Punjabi, India(Gurmukhi)"
    , "Punjabi, Pakistan(Perso-Arabic)"
    , "Punjabi, Pakistan(Devanagari)"
    , "Papiamento"
    , "Papuan Language"
    , "Northern Sotho"
    , "Northern Sotho, South Africa"
    , "Persian"
    , "Persian, Afghanistan"
    , "Persian(Perso-Arabic)"
    , "Persian(Cyrillic)"
    , "Persian, Iran"
    , "Philippine Language"
    , "Pohnpeian"
    , "Polish"
    , "Polish, Poland"
    , "Portuguese"
    , "Portuguese-based Creole or Pidgin"
    , "Portuguese, Brazil"
    , "Portuguese, Portugal"
    , "Prakrit Language"
    , "Punjabi"
    , "Punjabi, Pakistan"
    , "Pushto"
    , "Pushto, Afghanistan"
    , "Pushto(Perso-Arabic)"
    , "Quechua"
    , "Rajasthani"
    , "Rajasthani(Perso-Arabic)"
    , "Rajasthani(Devanagari)"
    , "Rapanui"
    , "Rarotongan"
    , "Romance Language"
    , "Romanian"
    , "Romanian, Moldova"
    , "Romanian, Romania"
    , "Romansh"
    , "Romany"
    , "Rundi"
    , "Russian"
    , "Russian, Russia"
    , "Russian, Ukraine"
    , "Russian, Kazakhstan"
    , "Salishan Language"
    , "Samaritan Aramaic"
    , "Samaritan Aramaic(Syriac)"
    , "Sami Language"
    , "Samoan"
    , "Sandawe"
    , "Sango"
    , "Sanskrit"
    , "Sanskrit, India"
    , "Santali"
    , "Santali(Bengali)"
    , "Santali(Devanagari)"
    , "Santali(Latin)"
    , "Santali(Oriya)"
    , "Sardinian"
    , "Sasak"
    , "Scots"
    , "Selkup"
    , "Semitic Language"
    , "Serbian"
    , "Serbian, Bosnia and Herzegovina"
    , "Serbian, Bosnia and Herzegovina(Cyrillic)"
    , "Serbian, Bosnia and Herzegovina(Latin)"
    , "Serbian(Cyrillic)"
    , "Serbian(Latin)"
    , "Serbian, Montenegro"
    , "Serbian, Montenegro(Cyrillic)"
    , "Serbian, Montenegro(Latin)"
    , "Serbian, Serbia"
    , "Serbian, Serbia and Montenegro"
    , "Serbian, Serbia and Montenegro(Cyrillic)"
    , "Serbian, Serbia and Montenegro(Latin)"
    , "Serbian, Serbia(Cyrillic)"
    , "Serbian, Serbia(Latin)"
    , "Serbo-Croatian"
    , "Serbo-Croatian, Bosnia and Herzegovina"
    , "Serbo-Croatian, Montenegro"
    , "Serbo-Croatian, Serbia and Montenegro"
    , "Serer"
    , "Serer(Perso-Arabic)"
    , "Serer(Latin)"
    , "Shan"
    , "Shona"
    , "Sichuan Yi"
    , "Sichuan Yi, China"
    , "Sichuan Yi, China(Yi)"
    , "Sichuan Yi(Yi)"
    , "Sicilian"
    , "Sidamo"
    , "Sidamo, Ethiopia"
    , "Sidamo(Ethiopic)"
    , "Sidamo(Latin)"
    , "Sign Language"
    , "Siksika"
    , "Sindhi"
    , "Sindhi(Perso-Arabic)"
    , "Sindhi(Devanagari)"
    , "Sindhi(Gurmukhi)"
    , "Sinhala"
    , "Sinhala, Sri Lanka"
    , "Sino-Tibetan Language"
    , "Siouan Language"
    , "Skolt Sami"
    , "Slave"
    , "Slavic Language"
    , "Slovak"
    , "Slovak, Slovakia"
    , "Slovenian"
    , "Slovenian, Slovenia"
    , "Sogdien"
    , "Somali"
    , "Somali(Perso-Arabic)"
    , "Somali, Djibouti"
    , "Somali, Ethiopia"
    , "Somali, Kenya"
    , "Somali, Somalia"
    , "Songhai"
    , "Soninke"
    , "Soninke(Perso-Arabic)"
    , "Soninke(Latin)"
    , "Sorbian Language"
    , "Southern Sotho"
    , "Southern Sotho, Lesotho"
    , "Southern Sotho, South Africa"
    , "South American Indian Language"
    , "Southern Altai"
    , "Southern Sami"
    , "Spanish"
    , "Spanish, Argentina"
    , "Spanish, Bolivia"
    , "Spanish, Chile"
    , "Spanish, Colombia"
    , "Spanish, Costa Rica"
    , "Spanish, Dominican Republic"
    , "Spanish, Ecuador"
    , "Spanish, El Salvador"
    , "Spanish, Guatemala"
    , "Spanish, Honduras"
    , "Spanish, Mexico"
    , "Spanish, Nicaragua"
    , "Spanish, Panama"
    , "Spanish, Paraguay"
    , "Spanish, Peru"
    , "Spanish, Puerto Rico"
    , "Spanish, Spain"
    , "Spanish, United States"
    , "Spanish, Uruguay"
    , "Spanish, Venezuela"
    , "Sranan Tongo"
    , "Sukuma"
    , "Sumerian"
    , "Sundanese"
    , "Sundanese(Perso-Arabic)"
    , "Sundanese(Javanese)"
    , "Sundanese(Latin)"
    , "Susu"
    , "Susu(Perso-Arabic)"
    , "Susu(Latin)"
    , "Swahili"
    , "Swahili, Kenya"
    , "Swahili, Tanzania"
    , "Swati"
    , "Swati, South Africa"
    , "Swati, Swaziland"
    , "Swedish"
    , "Swedish, Finland"
    , "Swedish, Sweden"
    , "Swiss German"
    , "Swiss German, Switzerland"
    , "Syriac"
    , "Syriac(Cyrillic)"
    , "Syriac, Syria"
    , "Syriac(Syriac)"
    , "Syriac, Syria(Cyrillic)"
    , "Tagalog"
    , "Tahitian"
    , "Tai Language"
    , "Tajik"
    , "Tajik(Perso-Arabic)"
    , "Tajik(Cyrillic)"
    , "Tajik(Latin)"
    , "Tajik, Tajikistan"
    , "Tajik, Tajikistan(Perso-Arabic)"
    , "Tajik, Tajikistan(Cyrillic)"
    , "Tajik, Tajikistan(Latin)"
    , "Tamashek"
    , "Tamashek(Perso-Arabic)"
    , "Tamashek(Latin)"
    , "Tamashek(Tifinagh)"
    , "Tamil"
    , "Tamil, India"
    , "Tatar"
    , "Tatar(Cyrillic)"
    , "Tatar(Latin)"
    , "Tatar, Russia"
    , "Tatar, Russia(Cyrillic)"
    , "Tatar, Russia(Latin)"
    , "Telugu"
    , "Telugu, India"
    , "Tereno"
    , "Tetum"
    , "Thai"
    , "Thai, Thailand"
    , "Tibetan"
    , "Tibetan, China"
    , "Tibetan, India"
    , "Tigre"
    , "Tigre, Eritrea"
    , "Tigrinya"
    , "Tigrinya, Eritrea"
    , "Tigrinya, Ethiopia"
    , "Timne"
    , "Tiv"
    , "Tlingit"
    , "Tokelau"
    , "Tok Pisin"
    , "Nyasa Tonga"
    , "Nyasa Tonga, Tonga"
    , "Tonga"
    , "Tsimshian"
    , "Tsimshian, South Africa"
    , "Tsonga"
    , "Tswana"
    , "Tswana, South Africa"
    , "Tumbuka"
    , "Tupi Language"
    , "Turkish"
    , "Turkish, Turkey"
    , "Turkmen"
    , "Turkmen(Perso-Arabic)"
    , "Turkmen(Cyrillic)"
    , "Turkmen(Latin)"
    , "Tuvalu"
    , "Tuvinian"
    , "Twi"
    , "Tyap"
    , "Tyap, Nigeria"
    , "Udmurt"
    , "Udmurt(Cyrillic)"
    , "Udmurt(Latin)"
    , "Ugaritic"
    , "Uyghur"
    , "Uyghur(Perso-Arabic)"
    , "Uyghur, China"
    , "Uyghur, China(Perso-Arabic)"
    , "Uyghur, China(Cyrillic)"
    , "Uyghur, China(Latin)"
    , "Uyghur(Cyrillic)"
    , "Uyghur(Latin)"
    , "Ukrainian"
    , "Ukrainian, Ukraine"
    , "Umbundu"
    , "Miscellaneous Language"
    , "Unknown Language"
    , "Upper Sorbian"
    , "Urdu"
    , "Urdu(Perso-Arabic)"
    , "Urdu, India"
    , "Urdu, Pakistan"
    , "Uzbek"
    , "Uzbek, Afghanistan"
    , "Uzbek, Afghanistan(Perso-Arabic)"
    , "Uzbek(Perso-Arabic)"
    , "Uzbek(Cyrillic)"
    , "Uzbek(Latin)"
    , "Uzbek, Uzbekistan"
    , "Uzbek, Uzbekistan(Cyrillic)"
    , "Uzbek, Uzbekistan(Latin)"
    , "Vai"
    , "Venda"
    , "Venda, South Africa"
    , "Vietnamese"
    , "Vietnamese, Vietnam"
    , "Volapük"
    , "Votic"
    , "Wakashan Language"
    , "Walamo"
    , "Walamo, Ethiopia"
    , "Walloon"
    , "Waray"
    , "Washo"
    , "Welsh"
    , "Welsh, United Kingdom"
    , "Western Frisian"
    , "Wolof"
    , "Wolof(Perso-Arabic)"
    , "Wolof(Latin)"
    , "Wolof, Senegal"
    , "Wolof, Senegal(Perso-Arabic)"
    , "Wolof, Senegal(Latin)"
    , "Xhosa"
    , "Xhosa, South Africa"
    , "Yakut"
    , "Yao"
    , "Yapese"
    , "Yiddish"
    , "Yiddish(Hebrew)"
    , "Yoruba"
    , "Yoruba, Nigeria"
    , "Yupik Language"
    , "Zande"
    , "Zapotec"
    , "Zaza"
    , "Zenaga"
    , "Zhuang"
    , "Zulu"
    , "Zulu, South Africa"
    , "Zuni"
    ]
