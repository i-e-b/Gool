using Gool;

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
    public static readonly BNF.Package Parser = Email();

    private static BNF.Package Email()
    {
        var _mailbox = BNF.Forward();
        
        BNF alpha = BNF.Regex("[a-zA-Z]");
        BNF digit = BNF.Regex("[0-9]");
        BNF symbol = BNF.OneOf('!','#','$','%','&','\'','*','+','-','/','=','?','^','_','`','{','|','}','~');
        BNF atext = alpha | digit | symbol;

        BNF atom = +atext;
        BNF dot_atom = atom % '.';

        BNF escape_char = '\\' > ((BNF)'\\' | '"');
        BNF quoted_string = '"' > -(escape_char | BNF.RangeExcluding('!', '~', '"', '\\')) > '"'; // ASCII printables, excluding double-quote or backslash.
        BNF domain_text = BNF.RangeExcluding('!', '~', '[', ']', '\\', '@');
        BNF word = atom | quoted_string;
        BNF phrase = +word;

        BNF ipv4_octet = BNF.IntegerRange(0, 255);
        BNF ipv4_address = ipv4_octet > '.' > ipv4_octet > '.' > ipv4_octet > '.' > ipv4_octet;
        
        BNF domain_literal = '[' > (-domain_text) > ']';
        BNF starts_with_alphanum = BNF.Regex("^[a-zA-Z].*");
        BNF ends_with_alphanum = BNF.Regex(".*[a-zA-Z]$");
        BNF dns_length_limit = BNF.RemainingLength(min:1, max:253);
        BNF contains_dot = BNF.Regex(".*\\..*");
        BNF domain_name = dot_atom.WithValidators(starts_with_alphanum, ends_with_alphanum, dns_length_limit, contains_dot);
        BNF domain = domain_name | domain_literal | ipv4_address;
        
        BNF local_part = (atom | quoted_string) % '.';
        BNF addr_spec = local_part > '@' > domain;
        
        BNF mailbox_list = _mailbox % ',';
        BNF group_list = mailbox_list;
        BNF display_name = phrase;
        BNF group = display_name > ':' > (!group_list) > ';';
        BNF angle_addr = '<' > addr_spec > '>';
        BNF name_addr = (!display_name) > angle_addr;
        BNF mailbox = name_addr | addr_spec;
        BNF address = mailbox | group;

        _mailbox.Is(mailbox);

        addr_spec.TagWith(Address);
        domain.TagWith(Domain);
        local_part.TagWith(User);
        
        return address.WithOptions(BNF.Options.SkipWhitespace);
    }

    public const string Address = "Address";
    public const string Domain = "Domain";
    public const string User = "User";
}