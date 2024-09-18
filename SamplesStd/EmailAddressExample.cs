using Gool;
using static Gool.BNF;

// ReSharper disable InconsistentNaming

namespace Samples;

/// <summary>
/// Based on https://datatracker.ietf.org/doc/html/rfc5322#section-3.4
/// <p/>
/// Yet another very broad standard that has a much smaller subset that is actually used.
/// This does not include the obsolete addressing forms
/// </summary>
public static class EmailAddressExample
{
    public static readonly Package Parser = Email();

    private static Package Email()
    {
        var _mailbox = Forward();

        BNF // Fragments
            alpha    = Regex("[a-zA-Z]"),
            digit    = Regex("[0-9]"),
            symbol   = OneOf('!', '#', '$', '%', '&', '\'', '*', '+', '-', '/', '=', '?', '^', '_', '`', '{', '|', '}', '~'),
            chr      = alpha | digit | symbol,
            atom     = +chr,
            dot_atom = atom % '.';

        BNF // String literals
            escape_char   = '\\' > ((BNF)'\\' | '"'),
            quoted_string = '"' > -(escape_char | RangeExcluding('!', '~', '"', '\\')) > '"', // ASCII printables, excluding double-quote or backslash.
            domain_text   = RangeExcluding('!', '~', '[', ']', '\\', '@'),
            word          = atom | quoted_string,
            phrase        = +word,
            local_part    = (atom | quoted_string) % '.';

        BNF // IP Addresses
            ipv4_octet   = IntegerRange(0, 255),
            ipv4_address = ipv4_octet > '.' > ipv4_octet > '.' > ipv4_octet > '.' > ipv4_octet;

        BNF // Domains
            domain_literal       = '[' > (-domain_text) > ']',
            starts_with_alphanum = Regex("^[a-zA-Z].*"),
            ends_with_alphanum   = Regex(".*[a-zA-Z]$"),
            dns_length_limit     = RemainingLength(min: 1, max: 253),
            contains_dot         = Regex(".*\\..*"),
            domain_name          = dot_atom.WithValidators(starts_with_alphanum, ends_with_alphanum, dns_length_limit, contains_dot),
            domain               = domain_name | domain_literal | ipv4_address;

        BNF // Sub-units
            addr_spec    = local_part > '@' > domain,
            mailbox_list = _mailbox % ',',
            group_list   = mailbox_list,
            display_name = phrase,
            group        = display_name > ':' > (!group_list) > ';',
            angle_addr   = '<' > addr_spec > '>',
            name_addr    = (!display_name) > angle_addr,
            mailbox      = name_addr | addr_spec,
            address      = mailbox | group;

        _mailbox.Is(mailbox);

        addr_spec.TagWith(Address);
        domain.TagWith(Domain);
        local_part.TagWith(User);

        return address.WithOptions(Options.SkipWhitespace);
    }

    public const string Address = "Address";
    public const string Domain  = "Domain";
    public const string User    = "User";
}