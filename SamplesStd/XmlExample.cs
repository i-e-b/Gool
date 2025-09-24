using Gool;
using static Gool.BNF;

// ReSharper disable MemberCanBePrivate.Global

// ReSharper disable InconsistentNaming

namespace Samples;

public static class XmlExample
{
    #region tags
    public const string Text                  = "text";
    public const string OpenElement               = "open";
    public const string CloseElement              = "close";
    public const string EmptyTag              = "empty";
    public const string TagId                 = "tagId";
    public const string Attribute             = "attribute";
    public const string Comment               = "comment";
    public const string ProcessingInstruction = "processingInstruction";
    public const string WhitespaceTag         = "whitespace";
    public const string DocumentType          = "documentType";
    #endregion tags

    /// <summary>
    /// "SimpleXmlParser" isn't a serious parser -- it can't handle real world XML.
    /// But it does show off simple parsing of a recursive data structure
    /// </summary>
    public static ParserPackage SimpleXmlParser()
    {
        BNF // Fragments
            text       = StringTerminatedBy("<"),
            identifier = IdentifierString(),
            whitespace = WhiteSpaceString;

        BNF // Literals
            quoted_string = '"' > identifier > '"',
            attribute     = whitespace > identifier > '=' > quoted_string;

        BNF // tags
            tag_id    = identifier.Tagged(TagId),
            open_tag  = '<' > tag_id > -attribute > '>',
            close_tag = "</" > tag_id > '>';

        attribute.TagWith(Attribute);
        text.TagWith(Text);
        open_tag.TagWith(OpenElement).OpenScope();
        close_tag.TagWith(CloseElement).CloseScope();

        return Recursive(tree => -(open_tag > -(tree | text) > close_tag))
            .Build();
    }

    /// <summary>
    /// This is another toy parser that shows how to use a <see cref="BNF.Context"/>
    /// to correctly match the opening and closing tags without a post-process.
    /// </summary>
    public static ParserPackage ContextualXmlParser()
    {

        var _wrapped = Forward();
        BNF
            identifier    = IdentifierString(allowUnderscore: false),
            whitespace    = WhiteSpaceString,

            element_id    = identifier.Tagged(TagId),

            text          = StringTerminatedBy("<"),
            quoted_string = '"' > identifier > '"',
            attribute     = whitespace > identifier > '=' > quoted_string,

            open_elem     = '<' > element_id > -attribute > '>',

            wrapped = // use the opening tag to make a contextual parser to match the end tag exactly.
                Context(
                    prefix: open_elem,
                    select: result =>
                        result.GetTag(TagId),
                    next: tag =>
                        -( text | _wrapped) >
                        ((BNF)"</" > tag.Value > '>').TagWith(CloseElement).CloseScope()
                );

        _wrapped.Is(wrapped);
        open_elem.TagWith(OpenElement).OpenScope();
        attribute.TagWith(Attribute);
        text.TagWith(Text);

        return wrapped.Build();
    }

    /*
     * A proper XML syntax below.
     *
     * The acceptable characters are explicitly enumerated.
     * This shows an advantage of having all the scopes and code folding
     * of the host language
     *
     */

    #region XML spec character classes

    private static readonly BNF XmlSpecBaseChar = CharacterInRanges(
        (0x41, 0x5A), (0x61, 0x7A), (0xC0, 0xD6)
      , (0xD8, 0xF6), (0xF8, 0xFF), (0x100, 0x131)
      , (0x134, 0x13E), (0x141, 0x148), (0x14A, 0x17E)
      , (0x180, 0x1C3), (0x1CD, 0x1F0), (0x1F4, 0x1F5)
      , (0x1FA, 0x217), (0x250, 0x2A8), (0x2BB, 0x2C1)
      , 0x386, (0x388, 0x38A), 0x38C, (0x38E, 0x3A1)
      , (0x3A3, 0x3CE), (0x3D0, 0x3D6), 0x3DA, 0x3DC
      , 0x3DE, 0x3E0, (0x3E2, 0x3F3), (0x401, 0x40C)
      , (0x40E, 0x44F), (0x451, 0x45C), (0x45E, 0x481)
      , (0x490, 0x4C4), (0x4C7, 0x4C8), (0x4CB, 0x4CC)
      , (0x4D0, 0x4EB), (0x4EE, 0x4F5), (0x4F8, 0x4F9)
      , (0x531, 0x556), 0x559, (0x561, 0x586)
      , (0x5D0, 0x5EA), (0x5F0, 0x5F2), (0x621, 0x63A)
      , (0x641, 0x64A), (0x671, 0x6B7), (0x6BA, 0x6BE)
      , (0x6C0, 0x6CE), (0x6D0, 0x6D3), 0x6D5, (0x6E5, 0x6E6)
      , (0x905, 0x939), 0x93D, (0x958, 0x961), (0x985, 0x98C)
      , (0x98F, 0x990), (0x993, 0x9A8), (0x9AA, 0x9B0)
      , 0x9B2, (0x9B6, 0x9B9), (0x9DC, 0x9DD), (0x9DF, 0x9E1)
      , (0x9F0, 0x9F1), (0xA05, 0xA0A), (0xA0F, 0xA10)
      , (0xA13, 0xA28), (0xA2A, 0xA30), (0xA32, 0xA33)
      , (0xA35, 0xA36), (0xA38, 0xA39), (0xA59, 0xA5C)
      , 0xA5E, (0xA72, 0xA74), (0xA85, 0xA8B), 0xA8D
      , (0xA8F, 0xA91), (0xA93, 0xAA8), (0xAAA, 0xAB0)
      , (0xAB2, 0xAB3), (0xAB5, 0xAB9), 0xABD, 0xAE0
      , (0xB05, 0xB0C), (0xB0F, 0xB10), (0xB13, 0xB28)
      , (0xB2A, 0xB30), (0xB32, 0xB33), (0xB36, 0xB39)
      , 0xB3D, (0xB5C, 0xB5D), (0xB5F, 0xB61)
      , (0xB85, 0xB8A), (0xB8E, 0xB90), (0xB92, 0xB95)
      , (0xB99, 0xB9A), 0xB9C, (0xB9E, 0xB9F)
      , (0xBA3, 0xBA4), (0xBA8, 0xBAA), (0xBAE, 0xBB5)
      , (0xBB7, 0xBB9), (0xC05, 0xC0C), (0xC0E, 0xC10)
      , (0xC12, 0xC28), (0xC2A, 0xC33), (0xC35, 0xC39)
      , (0xC60, 0xC61), (0xC85, 0xC8C), (0xC8E, 0xC90)
      , (0xC92, 0xCA8), (0xCAA, 0xCB3), (0xCB5, 0xCB9)
      , 0xCDE, (0xCE0, 0xCE1), (0xD05, 0xD0C), (0xD0E, 0xD10)
      , (0xD12, 0xD28), (0xD2A, 0xD39), (0xD60, 0xD61)
      , (0xE01, 0xE2E), 0xE30, (0xE32, 0xE33), (0xE40, 0xE45)
      , (0xE81, 0xE82), 0xE84, (0xE87, 0xE88), 0xE8A
      , 0xE8D, (0xE94, 0xE97), (0xE99, 0xE9F), (0xEA1, 0xEA3)
      , 0xEA5, 0xEA7, (0xEAA, 0xEAB), (0xEAD, 0xEAE), 0xEB0
      , (0xEB2, 0xEB3), 0xEBD, (0xEC0, 0xEC4), (0xF40, 0xF47)
      , (0xF49, 0xF69), (0x10A0, 0x10C5), (0x10D0, 0x10F6), 0x1100
      , (0x1102, 0x1103), (0x1105, 0x1107), 0x1109, (0x110B, 0x110C)
      , (0x110E, 0x1112), 0x113C, 0x113E, 0x1140, 0x114C, 0x114E
      , 0x1150, (0x1154, 0x1155), 0x1159, (0x115F, 0x1161), 0x1163
      , 0x1165, 0x1167, 0x1169, (0x116D, 0x116E), (0x1172, 0x1173)
      , 0x1175, 0x119E, 0x11A8, 0x11AB, (0x11AE, 0x11AF)
      , (0x11B7, 0x11B8), 0x11BA, (0x11BC, 0x11C2), 0x11EB, 0x11F0
      , 0x11F9, (0x1E00, 0x1E9B), (0x1EA0, 0x1EF9), (0x1F00, 0x1F15)
      , (0x1F18, 0x1F1D), (0x1F20, 0x1F45), (0x1F48, 0x1F4D)
      , (0x1F50, 0x1F57), 0x1F59, 0x1F5B, 0x1F5D, (0x1F5F, 0x1F7D)
      , (0x1F80, 0x1FB4), (0x1FB6, 0x1FBC), 0x1FBE, (0x1FC2, 0x1FC4)
      , (0x1FC6, 0x1FCC), (0x1FD0, 0x1FD3), (0x1FD6, 0x1FDB)
      , (0x1FE0, 0x1FEC), (0x1FF2, 0x1FF4), (0x1FF6, 0x1FFC), 0x2126
      , (0x212A, 0x212B), 0x212E, (0x2180, 0x2182), (0x3041, 0x3094)
      , (0x30A1, 0x30FA), (0x3105, 0x312C), (0xAC00, 0xD7A3)
    );

    private static readonly BNF XmlSpecIdeographic = CharacterInRanges((0x4E00, 0x9FA5), 0x3007, (0x3021, 0x3029));

    private static readonly BNF XmlSpecCombining = CharacterInRanges(
        (0x300, 0x345), (0x360, 0x361), (0x483, 0x486)
      , (0x591, 0x5A1), (0x5A3, 0x5B9), (0x5BB, 0x5BD), 0x5BF
      , (0x5C1, 0x5C2), 0x5C4, (0x64B, 0x652), 0x670
      , (0x6D6, 0x6DC), (0x6DD, 0x6DF), (0x6E0, 0x6E4)
      , (0x6E7, 0x6E8), (0x6EA, 0x6ED), (0x901, 0x903)
      , 0x93C, (0x93E, 0x94C), 0x94D, (0x951, 0x954)
      , (0x962, 0x963), (0x981, 0x983), 0x9BC, 0x9BE
      , 0x9BF, (0x9C0, 0x9C4), (0x9C7, 0x9C8), (0x9CB, 0x9CD)
      , 0x9D7, (0x9E2, 0x9E3), 0xA02, 0xA3C, 0xA3E, 0xA3F
      , (0xA40, 0xA42), (0xA47, 0xA48), (0xA4B, 0xA4D)
      , (0xA70, 0xA71), (0xA81, 0xA83), 0xABC, (0xABE, 0xAC5)
      , (0xAC7, 0xAC9), (0xACB, 0xACD), (0xB01, 0xB03), 0xB3C
      , (0xB3E, 0xB43), (0xB47, 0xB48), (0xB4B, 0xB4D)
      , (0xB56, 0xB57), (0xB82, 0xB83), (0xBBE, 0xBC2)
      , (0xBC6, 0xBC8), (0xBCA, 0xBCD), 0xBD7, (0xC01, 0xC03)
      , (0xC3E, 0xC44), (0xC46, 0xC48), (0xC4A, 0xC4D)
      , (0xC55, 0xC56), (0xC82, 0xC83), (0xCBE, 0xCC4)
      , (0xCC6, 0xCC8), (0xCCA, 0xCCD), (0xCD5, 0xCD6)
      , (0xD02, 0xD03), (0xD3E, 0xD43), (0xD46, 0xD48)
      , (0xD4A, 0xD4D), 0xD57, 0xE31, (0xE34, 0xE3A)
      , (0xE47, 0xE4E), 0xEB1, (0xEB4, 0xEB9), (0xEBB, 0xEBC)
      , (0xEC8, 0xECD), (0xF18, 0xF19), 0xF35, 0xF37, 0xF39
      , 0xF3E, 0xF3F, (0xF71, 0xF84), (0xF86, 0xF8B)
      , (0xF90, 0xF95), 0xF97, (0xF99, 0xFAD), (0xFB1, 0xFB7)
      , 0xFB9, (0x20D0, 0x20DC), 0x20E1, (0x302A, 0x302F)
      , 0x3099, 0x309A
    );

    private static readonly BNF XmlSpecDigit = CharacterInRanges(
        (0x30, 0x39), (0x660, 0x669), (0x6F0, 0x6F9)
      , (0x966, 0x96F), (0x9E6, 0x9EF), (0xA66, 0xA6F)
      , (0xAE6, 0xAEF), (0xB66, 0xB6F), (0xBE7, 0xBEF)
      , (0xC66, 0xC6F), (0xCE6, 0xCEF), (0xD66, 0xD6F)
      , (0xE50, 0xE59), (0xED0, 0xED9), (0xF20, 0xF29)
    );

    private static readonly BNF XmlSpecExtender = CharacterInRanges(
        0xB7, 0x2D0, 0x2D1, 0x387, 0x640, 0xE46
      , 0xEC6, 0x3005, (0x3031, 0x3035), (0x309D, 0x309E)
      , (0x30FC, 0x30FE)
    );

    private static readonly BNF XmlSpecChar = CharacterInRanges(
        0x9, 0xA, 0xD, (0x20, 0xD7FF), (0xE000, 0xFFFD), (0x10000, 0x10FFFF)
    );

    #endregion XML spec character classes

    /// <summary>
    /// Based on https://cs.lmu.edu/~ray/notes/xmlgrammar/
    /// which consolidates https://www.w3.org/TR/REC-xml plus some errata.
    /// </summary>
    public static ParserPackage FullXmlParser()
    {
        BNF // Character ranges and whitespace (XML spec is very specific about ranges)
            ws             = +(OneOf(' ', '\t', '\n', '\r')),
            chr            = XmlSpecChar,
            digit          = XmlSpecDigit,
            combining_char = XmlSpecCombining,
            extender       = XmlSpecExtender,
            letter         = XmlSpecBaseChar | XmlSpecIdeographic;

        BNF // Fragments
            eq = !ws > '=' > !ws;

        BNF // Names and tokens
            name_char = letter | digit | OneOf('.', '-', ':') | combining_char | extender,
            name      = (letter | '_' | ':') > -(name_char),
            nm_token  = +name_char;

        BNF // Character and Entity References
            char_ref     = ("&#" > (+DecimalDigit) > ';') | ("&#x" > (+HexDigit) > ';'),
            entity_ref   = '&' > name > ';',
            reference    = entity_ref | char_ref,
            pe_reference = '%' > name > ';';

        BNF // Literals
            entity_value = '"' > -(NoneOf('%', '&', '"') | pe_reference | reference) > '"'
                         | "'" > -(NoneOf('%', '&', '\'') | pe_reference | reference) > '"',
            att_value = '"' > -(NoneOf('<', '&', '"') | reference) > '"'
                      | "'" > -(NoneOf('<', '&', '\'') | reference) > "'",
            system_literal = ('"' > (-NoneOf('"')) > '"') | ("'" > (-NoneOf('\'')) > "'"),
            pub_id_char    = CharacterInRanges(0x20, 0x0D, 0x0A, ('a', 'z'), ('A', 'Z'), ('0', '9'), '-', '\'', '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*', '#', '@', '$', '_', '%'),
            pub_id_literal = ('"' > (-pub_id_char) > '"');

        BNF // Character data
            char_data = -(NoneOf('<', '&') / "]]>");

        BNF // Comments
            comment = "<!--" > -((chr / '-') | ('-' > (chr / '-'))) > "-->";

        BNF // Processing Instructions
            pi_target     = name / ("xml ".CaseInsensitive() | "xml?".CaseInsensitive()),
            proc_instruct = "<?" > pi_target > !(ws > -(chr / "?>")) > "?>";

        BNF // CDATA Sections
            cd_lead       = "<![",
            cd_start      = "<![CDATA[",
            cd_end        = "]]>",
            raw_char_data = -(chr / cd_end),
            cd_section    = cd_start > raw_char_data > cd_end;

        BNF // Standalone Document Declaration
            yes     = "yes",
            no      = "no",
            sd_decl = ws > "standalone" > eq > (("'" > (yes | no) > "'") | ('"' > (yes | no) > '"'));

        var _element = Forward();
        BNF // Elements, Tags and Element Content
            attribute      = name > eq > att_value,
            empty_elem_tag = '<' > name > -(ws > attribute) > -ws > "/>",
            start_tag      = '<' > name > -(ws > attribute) > -ws > '>',
            end_tag        = "</" > name > -ws > '>',
            content        = !char_data > -((_element | reference | cd_section | proc_instruct | comment) > !char_data),
            element        = empty_elem_tag | (start_tag > content > end_tag);
        _element.Is(element);

        var _choice = Forward();
        var _seq    = Forward();
        BNF // Elements in the DTD
            empty        = "EMPTY",
            any          = "ANY",
            mixed        = ('(' > !ws > "#PCDATA" > -(!ws > '|' > !ws > name) > !ws > ")*") | ('(' > !ws > "#PCDATA" > !ws > ')'),
            cp           = (name | _choice | _seq) > !OneOf('?', '*', '+'),
            seq          = '(' > !ws > cp > -(!ws > ',' > !ws > cp) > !ws > ')',
            choice       = '(' > !ws > cp > +(!ws > '|' > !ws > cp) > !ws > ')',
            children     = (choice | seq) > !OneOf('?', '*', '+'),
            content_spec = empty | any | mixed | children,
            element_decl = "<!ELEMENT" > ws > name > ws > content_spec > !ws > '>';
        _seq.Is(seq);
        _choice.Is(choice);

        BNF // Attributes in the DTD
            string_type     = "CDATA",
            tokenized_type  = (BNF)"ID" | "IDREF" | "IDREFS" | "ENTITY" | "ENTITIES" | "NMTOKEN" | "NMTOKENS",
            notation_type   = "NOTATION" > ws > '(' > !ws > name > -(!ws > '|' > !ws > name) > !ws > ')',
            enumeration     = '(' > !ws > nm_token > -(!ws > '|' > !ws > nm_token) > !ws > ')',
            enumerated_type = notation_type | enumeration,
            att_type        = string_type | tokenized_type | enumerated_type,
            default_decl    = (BNF)"#REQUIRED" | "#IMPLIED" | (!("#FIXED" > !ws) > att_value),
            att_def         = ws > name > ws > att_type > ws > default_decl,
            att_list_decl   = "<!ATTLIST" > ws > name > -att_def > !ws > '>';

        BNF // Entity Declarations
            n_data_decl = ws > "NDATA" > ws > name,
            external_id = ("SYSTEM" > ws > system_literal) | ("PUBLIC" > ws > pub_id_literal > ws > system_literal),
            pe_def = entity_value | external_id,
            entity_def = entity_value | (external_id > n_data_decl),
            pe_decl = "<!ENTITY" > ws > '%' > ws > name > ws > pe_def > !ws > '>',
            ge_decl = "<!ENTITY" > ws > name > ws > entity_def > !ws > '>',
            entity_decl = ge_decl | pe_decl;

        var _version_info = Forward();
        BNF // Parsed Entities
            public_id     = "PUBLIC" > ws > pub_id_literal,
            notation_decl = "<!NOTATION" > ws > name > ws > (external_id | public_id) > !ws > '>',
            enc_name      = CharacterInRanges(('A', 'Z'), ('a', 'z')) > -CharacterInRanges(('A', 'Z'), ('a', 'z'), ('0', '9'), '.', '_', '-'),
            encoding_decl = ws > "encoding" > eq > (('"' > enc_name > '"') | ("'" > enc_name > "'"));

        var _conditional_sect = Forward();
        BNF // Document Type Definition
            decl_sep        = pe_reference | ws,
            markup_decl     = element_decl | att_list_decl | entity_decl | notation_decl | proc_instruct | comment,
            int_subset      = -(markup_decl | decl_sep),
            ext_subset_decl = -(markup_decl | _conditional_sect | decl_sep),
            //ext_subset      = (!text_decl) | ext_subset_decl, // Defined in "2.8 Prolog and Document Type Declaration", but not used in the syntax
            doc_type_decl   = "<!DOCTYPE" > ws > name > !(ws > external_id) > !ws > !('[' > int_subset > ']' > !ws) > '>';

        var _ignore_sect_contents = Forward();
        BNF // Conditional Section
            ignore               = -(chr / (cd_lead | cd_end)),
            ignore_sect_contents = ignore > -(cd_lead > _ignore_sect_contents > cd_end > ignore),
            ignore_sect          = cd_lead > !ws > "IGNORE" > !ws > '[' > (-ignore_sect_contents) > cd_end,
            include_sect         = cd_lead > !ws > "INCLUDE" > !ws > '[' > (-ext_subset_decl) > cd_end,
            conditional_sect     = include_sect | ignore_sect;
        _ignore_sect_contents.Is(ignore_sect_contents);
        _conditional_sect.Is(conditional_sect);

        BNF // Prolog
            misc         = comment | proc_instruct | ws,
            version_num  = "1.0",
            version_info = ws > "version" > eq > (("'" > version_num > "'") | ('"' > version_num > '"')),
            xml_decl     = "<?xml" > version_info > !encoding_decl > !sd_decl > !ws > "?>",
            prolog       = !xml_decl > (-misc) > !(doc_type_decl > (-misc));
        _version_info.Is(version_info);

        // An entire document
        BNF document = prolog > element > -misc;

        // Basic tagging
        comment.TagWith(Comment);
        proc_instruct.TagWith(ProcessingInstruction);
        ws.TagWith(WhitespaceTag);
        doc_type_decl.TagWith(DocumentType);

        char_data.TagWith(Text);
        raw_char_data.TagWith(Text);

        attribute.TagWith(Attribute);
        empty_elem_tag.TagWith(EmptyTag).EncloseScope();
        start_tag.TagWith(OpenElement).OpenScope();
        end_tag.TagWith(CloseElement).CloseScope();

        return document.Build();
    }

}