﻿using System.Globalization;
using System.Text;
using Gool;
using Gool.Parsers.Terminals;
using Gool.Scanners;
using NUnit.Framework;

namespace TestsStd;

[TestFixture]
public class TerminalBasicTests
{
    [Test]
    public void _AnyCharacter_()
    {
        var subject = new AnyCharacter();
        var chr = new char[1];

        for (int i = 0; i < 100; i++)
        {
            chr[0] = (char)Random.Shared.Next(0, (i + 1) * 100);
            var test = new string(chr);
            Console.Write(test);
            var result = subject.Parse(new ScanStrings(test), null);

            Assert.That(result.Success, Is.True);
            Assert.That(result.Length, Is.EqualTo(1));
        }
    }

    [Test]
    public void _EmptyMatch_()
    {
        var subject = new EmptyMatch();
        var chr = new char[1];

        for (int i = 0; i < 100; i++)
        {
            chr[0] = (char)Random.Shared.Next(0, (i + 1) * 100);
            var test = new string(chr);
            Console.Write(test);
            var result = subject.Parse(new ScanStrings(test), null);

            Assert.That(result.Success, Is.True);
            Assert.That(result.Length, Is.EqualTo(0));
        }
    }

    [Test]
    public void _EndOfInput_()
    {
        var subject = new EndOfInput();
        var input = "Hello, world!";

        var scanner = new ScanStrings(input);
        var nullParser = new NullParser("test");

        var result = subject.Parse(scanner, null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 0, 0, null));
        Assert.That(result.Success, Is.False);

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 0, 12, null));
        Assert.That(result.Success, Is.False);

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 12, 1, null));
        Assert.That(result.Success, Is.True);

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 13, 0, null));
        Assert.That(result.Success, Is.True);

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 0, 13, null));
        Assert.That(result.Success, Is.True);
    }

    [Test]
    public void _EndOfLine_()
    {
        var subject = new EndOfLine();
        //                       1
        //           0123 4567 890123 4 5678
        var input = "one\rtwo\nthree\r\nfour";

        var scanner = new ScanStrings(input);
        var nullParser = new NullParser("test");

        var result = subject.Parse(scanner, null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 1, 1, null));
        Assert.That(result.Success, Is.False);

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 12, 3, null));
        Assert.That(result.Success, Is.False);

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 3, 0, null));
        Assert.That(result.Success, Is.True);
        Assert.That(result.Length, Is.EqualTo(1));

        result = subject.Parse(scanner,scanner.CreateMatch(nullParser, 7, 0, null));
        Assert.That(result.Success, Is.True);
        Assert.That(result.Length, Is.EqualTo(1));

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 13, 0, null));
        Assert.That(result.Success, Is.True);
        Assert.That(result.Length, Is.EqualTo(2));

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 18, 0, null));
        Assert.That(result.Success, Is.False);
    }

    [Test]
    public void _ExcludingCharacterSet_()
    {
        var nullParser = new NullParser("test");
        var subject    = new ExcludingCharacterSet('i', 'e', 'b');
        var input      = "abcdefghijklmnop";
        var scanner    = new ScanStrings(input);

        var offset = 0;
        foreach (var c in input)
        {
            var result = subject.Parse(scanner, scanner.CreateMatch(nullParser, offset, 0, null));
            offset++;

            Console.Write(c);
            if (c is 'i' or 'e' or 'b')
            {
                Assert.That(result.Success, Is.False);
            }
            else
            {
                Assert.That(result.Success, Is.True);
                Assert.That(result.Length, Is.EqualTo(1));
            }
        }
    }

    [Test]
    public void _LiteralCharacterSet_()
    {
        var nullParser = new NullParser("test");
        var subject    = new LiteralCharacterSet('i', 'e', 'b');
        var input      = "abcdefghijklmnop";
        var scanner    = new ScanStrings(input);

        var offset = 0;
        foreach (var c in input)
        {
            var result = subject.Parse(scanner, scanner.CreateMatch(nullParser, offset, 0, null));
            offset++;

            Console.Write(c);
            if (c is 'i' or 'e' or 'b')
            {
                Assert.That(result.Success, Is.True);
                Assert.That(result.Length, Is.EqualTo(1));
            }
            else
            {
                Assert.That(result.Success, Is.False);
            }
        }
    }

    [Test]
    public void _LiteralString_()
    {
        var subject = new LiteralString("three");
        //                       1
        //           012345678901234567
        var input = "one two three four";

        var scanner = new ScanStrings(input);
        var nullParser = new NullParser("test");

        var result = subject.Parse(scanner, null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 1, 1, null));
        Assert.That(result.Success, Is.False);

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 1, 6, null));
        Assert.That(result.Success, Is.False);

        result = subject.Parse(scanner,scanner.CreateMatch(nullParser, 8, 0, null));
        Assert.That(result.Success, Is.True);
        Assert.That(result.Length, Is.EqualTo(5));

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 1, 7, null));
        Assert.That(result.Success, Is.True);
        Assert.That(result.Length, Is.EqualTo(5));

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 13, 0, null));
        Assert.That(result.Success, Is.False);
    }

    [Test]
    public void _LiteralString_insensitive_()
    {
        var subject = new LiteralString("three", StringComparison.OrdinalIgnoreCase);
        //                       1
        //           012345678901234567
        var input = "one two ThREe four";

        var scanner    = new ScanStrings(input);
        var nullParser = new NullParser("test");

        var result = subject.Parse(scanner, null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 1, 1, null));
        Assert.That(result.Success, Is.False);

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 1, 6, null));
        Assert.That(result.Success, Is.False);

        result = subject.Parse(scanner,scanner.CreateMatch(nullParser, 8, 0, null));
        Assert.That(result.Success, Is.True);
        Assert.That(result.Length, Is.EqualTo(5));

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 1, 7, null));
        Assert.That(result.Success, Is.True);
        Assert.That(result.Length, Is.EqualTo(5));

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 13, 0, null));
        Assert.That(result.Success, Is.False);
    }

    [Test]
    public void _UtfCategory_()
    {
        var subject = BNF.UtfCategory(UnicodeCategory.MathSymbol);

        var result  = subject.Parse(new ScanStrings("+"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("="));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("fish"));
        Assert.That(result.Success, Is.False);
    }

    [Test]
    public void _AlphaNumeric_()
    {
        var subject = BNF.AlphaNumeric;

        var result  = subject.Parse(new ScanStrings("A"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("q"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("5"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("?"));
        Assert.That(result.Success, Is.False);
    }

    [Test]
    public void _Uppercase_()
    {
        var subject = BNF.Uppercase;

        var result  = subject.Parse(new ScanStrings("A"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("Ш"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("5"));
        Assert.That(result.Success, Is.False);

        result  = subject.Parse(new ScanStrings("b"));
        Assert.That(result.Success, Is.False);
    }

    [Test]
    public void _Lowercase_()
    {
        var subject = BNF.Lowercase;

        var result  = subject.Parse(new ScanStrings("a"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("ш"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("5"));
        Assert.That(result.Success, Is.False);

        result  = subject.Parse(new ScanStrings("B"));
        Assert.That(result.Success, Is.False);
    }

    [Test]
    public void _Letter_()
    {
        var subject = BNF.Letter;

        var result  = subject.Parse(new ScanStrings("a"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("ш"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("B"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("5"));
        Assert.That(result.Success, Is.False);

        result  = subject.Parse(new ScanStrings("?"));
        Assert.That(result.Success, Is.False);
    }

    [Test]
    public void _Digit_()
    {
        var subject = BNF.Digit;

        var result  = subject.Parse(new ScanStrings("0"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("1"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("⅘"));
        Assert.That(result.Success, Is.False);

        result  = subject.Parse(new ScanStrings("."));
        Assert.That(result.Success, Is.False);

        result  = subject.Parse(new ScanStrings("q"));
        Assert.That(result.Success, Is.False);
    }

    [Test]
    public void _HexDigit_()
    {
        var subject = BNF.HexDigit;

        var result  = subject.Parse(new ScanStrings("0"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("1"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("A"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("f"));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("q"));
        Assert.That(result.Success, Is.False);
    }

    [Test]
    public void _RegularExpression_()
    {
        var subject = new RegularExpression("(^one)|(one$)");
        //           01234567890
        var input = "one one one";

        var scanner = new ScanStrings(input);
        var nullParser = new NullParser("test");

        var result = subject.Parse(scanner, null);
        Assert.That(result.Success, Is.True);
        Assert.That(result.Length, Is.EqualTo(3));

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 0, 0, null));
        Assert.That(result.Success, Is.True);
        Assert.That(result.Length, Is.EqualTo(3));

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 4, 0, null));
        Assert.That(result.Success, Is.False);

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 3, 1, null));
        Assert.That(result.Success, Is.False);

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 8, 0, null));
        Assert.That(result.Success, Is.True);
        Assert.That(result.Length, Is.EqualTo(3));

        result = subject.Parse(scanner, scanner.CreateMatch(nullParser, 6, 2, null));
        Assert.That(result.Success, Is.True);
        Assert.That(result.Length, Is.EqualTo(3));
    }

    [Test]
    public void _Whitespace_()
    {
        var nullParser = new NullParser("test");
        var subject    = new Whitespace();
        var input      = "abc def\tghi\rjkl\nmno";
        var scanner    = new ScanStrings(input);

        var offset = 0;
        foreach (var c in input)
        {
            var result = subject.Parse(scanner, scanner.CreateMatch(nullParser, offset, 0, null));
            offset++;

            Console.Write(c);
            if (char.IsWhiteSpace(c))
            {
                Assert.That(result.Success, Is.True);
                Assert.That(result.Length, Is.EqualTo(1));
            }
            else
            {
                Assert.That(result.Success, Is.False, $"Is this whitespace? '{c}'");
            }
        }
    }

    [Test]
    public void _WhiteSpaceCount_()
    {
        var subject = BNF.WhiteSpaceCount(2,4);

        var result  = subject.Parse(new ScanStrings(" "));
        Assert.That(result.Success, Is.False);

        result  = subject.Parse(new ScanStrings("  "));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("   "));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("     "));
        Assert.That(result.Success, Is.True);

        result  = subject.Parse(new ScanStrings("     "));
        Assert.That(result.Success, Is.True);
        Assert.That(result.Length, Is.EqualTo(4));
    }

    [Test]
    public void _FixedWidthIntegerRange_()
    {
        var subject = new FixedWidthIntegerRange(99, 444, 3, false, false);
        Console.WriteLine(subject.ToString());

        var result = subject.Parse(new ScanStrings("234"), null);
        Assert.That(result.Success, Is.True); // in range, correct length

        result = subject.Parse(new ScanStrings("001"), null);
        Assert.That(result.Success, Is.False); // out-of-range, correct length

        result = subject.Parse(new ScanStrings("999"), null);
        Assert.That(result.Success, Is.False); // out-of-range, correct length

        result = subject.Parse(new ScanStrings("99"), null);
        Assert.That(result.Success, Is.False); // in range, wrong length

        result = subject.Parse(new ScanStrings("0099"), null);
        Assert.That(result.Success, Is.False); // in range, wrong length

        result = subject.Parse(new ScanStrings(" 99"), null);
        Assert.That(result.Success, Is.False); // in range, correct length, wrong leader
        
        
        
        
        subject = new FixedWidthIntegerRange(32 /*0x20*/, 50 /* 0x32 */, 2, false, useHex:true);
        Console.WriteLine(subject.ToString());

        result = subject.Parse(new ScanStrings("33"), null);
        Assert.That(result.Success, Is.False); // correct length, would be in range if dec, but out-of-range for hex
        
        result = subject.Parse(new ScanStrings("20"), null);
        Assert.That(result.Success, Is.True); // correct length, would be out-of-range if dec, but in range for hex

        result = subject.Parse(new ScanStrings("FF"), null);
        Assert.That(result.Success, Is.False); // out-of-range, correct length

        result = subject.Parse(new ScanStrings("01"), null);
        Assert.That(result.Success, Is.False); // out-of-range, correct length

        result = subject.Parse(new ScanStrings("030"), null);
        Assert.That(result.Success, Is.False); // in range, wrong length
        
        
        
        subject = new FixedWidthIntegerRange(1, 365, 3, allowLeadingWhitespace: true, useHex:false);
        Console.WriteLine(subject.ToString());

        result = subject.Parse(new ScanStrings("  3"), null);
        Assert.That(result.Success, Is.True); // correct length, in range
        
        result = subject.Parse(new ScanStrings("003"), null);
        Assert.That(result.Success, Is.True); // correct length, in range

        result = subject.Parse(new ScanStrings(" 03"), null);
        Assert.That(result.Success, Is.True); // correct length, in range

        result = subject.Parse(new ScanStrings("999"), null);
        Assert.That(result.Success, Is.False); // out-of-range, correct length

        result = subject.Parse(new ScanStrings("5"), null);
        Assert.That(result.Success, Is.False); // in range, wrong length
        
        
        subject = new FixedWidthIntegerRange(1, 128, 3, allowLeadingWhitespace: true, useHex:true);
        Console.WriteLine(subject.ToString());

        result = subject.Parse(new ScanStrings("  3"), null);
        Assert.That(result.Success, Is.True); // correct length, in range
        
        result = subject.Parse(new ScanStrings("003"), null);
        Assert.That(result.Success, Is.True); // correct length, in range

        result = subject.Parse(new ScanStrings(" 7F"), null);
        Assert.That(result.Success, Is.True); // correct length, in range

        result = subject.Parse(new ScanStrings("128"), null);
        Assert.That(result.Success, Is.False); // out-of-range, correct length

        result = subject.Parse(new ScanStrings("5"), null);
        Assert.That(result.Success, Is.False); // in range, wrong length
    }
    
    [Test]
    public void _VariableWidthIntegerRange_()
    {
        var subject = new VariableWidthIntegerRange(99, 444, false, false);
        Console.WriteLine(subject.ToString());

        var result = subject.Parse(new ScanStrings("234!"), null);
        Assert.That(result.Success, Is.True); // in range, correct length

        result = subject.Parse(new ScanStrings("1"), null);
        Assert.That(result.Success, Is.False); // out-of-range

        result = subject.Parse(new ScanStrings("999"), null);
        Assert.That(result.Success, Is.False); // out-of-range

        result = subject.Parse(new ScanStrings("99"), null);
        Assert.That(result.Success, Is.True); // in range

        result = subject.Parse(new ScanStrings("0099"), null);
        Assert.That(result.Success, Is.True); // in range

        result = subject.Parse(new ScanStrings(" 99"), null);
        Assert.That(result.Success, Is.False); // in range, correct length, wrong leader
        
    }


    [Test]
    public void _VariableWidthFractionalDecimal_does_not_eat_trailing_E()
    {
        var subject = new VariableWidthFractionalDecimal(groupMark: "_", decimalMark: ".",
            allowLeadingWhitespace: false, allowLoneDecimal: false, allowLeadingZero: false, allowLeadingPlus: true);
        Console.WriteLine(subject.ToString());

        var result = subject.Parse(new ScanStrings("234e"), null);
        Assert.That(result.Value, Is.EqualTo("234"));
    }

    [Test]
    public void _VariableWidthFractionalDecimal_without_leading_whitespace()
    {
        var subject = new VariableWidthFractionalDecimal(groupMark: "_", decimalMark: ".",
            allowLeadingWhitespace: false, allowLoneDecimal: false, allowLeadingZero: false, allowLeadingPlus: true);
        Console.WriteLine(subject.ToString());

        var result = subject.Parse(new ScanStrings("234"), null);
        Assert.That(result.Value, Is.EqualTo("234"));

        result = subject.Parse(new ScanStrings("1.0"), null);
        Assert.That(result.Value, Is.EqualTo("1.0"));

        result = subject.Parse(new ScanStrings("0.1"), null);
        Assert.That(result.Value, Is.EqualTo("0.1"));

        result = subject.Parse(new ScanStrings("+99"), null);
        Assert.That(result.Value, Is.EqualTo("+99"));

        result = subject.Parse(new ScanStrings("-99"), null);
        Assert.That(result.Value, Is.EqualTo("-99"));

        result = subject.Parse(new ScanStrings("-9.0E5"), null);
        Assert.That(result.Value, Is.EqualTo("-9.0E5"));

        result = subject.Parse(new ScanStrings("+9.0E-5"), null);
        Assert.That(result.Value, Is.EqualTo("+9.0E-5"));

        result = subject.Parse(new ScanStrings("0.1E+5"), null);
        Assert.That(result.Value, Is.EqualTo("0.1E+5"));

        result = subject.Parse(new ScanStrings("123__000_000.0234e222"), null);
        Assert.That(result.Value, Is.EqualTo("123__000_000.0234e222"));

        result = subject.Parse(new ScanStrings("1_._0"), null);
        Assert.That(result.Value, Is.EqualTo("1_._0"));

        result = subject.Parse(new ScanStrings("0"), null);
        Assert.That(result.Value, Is.EqualTo("0"));

        result = subject.Parse(new ScanStrings("1"), null);
        Assert.That(result.Value, Is.EqualTo("1"));

        // Invalid cases

        result = subject.Parse(new ScanStrings("  1.0"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("."), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("+"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("-"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("++1"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("-+1"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings(".1"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("1."), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("00"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("_1.0"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("1.0_"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("1..0"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("1._.0"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("1.0e1.0"), null);
        Assert.That(result.Success, Is.False);
    }

    [Test]
    public void _VariableWidthFractionalDecimal_with_lone_decimal()
    {
        var subject = new VariableWidthFractionalDecimal(groupMark: "_", decimalMark: ".",
            allowLeadingWhitespace: false, allowLoneDecimal: true, allowLeadingZero: false, allowLeadingPlus: true);
        Console.WriteLine(subject.ToString());

        var result = subject.Parse(new ScanStrings("234"), null);
        Assert.That(result.Value, Is.EqualTo("234"));

        result = subject.Parse(new ScanStrings("1.0"), null);
        Assert.That(result.Value, Is.EqualTo("1.0"));

        result = subject.Parse(new ScanStrings("0.1"), null);
        Assert.That(result.Value, Is.EqualTo("0.1"));

        result = subject.Parse(new ScanStrings("+99"), null);
        Assert.That(result.Value, Is.EqualTo("+99"));

        result = subject.Parse(new ScanStrings("-99"), null);
        Assert.That(result.Value, Is.EqualTo("-99"));

        result = subject.Parse(new ScanStrings("-9.0E5"), null);
        Assert.That(result.Value, Is.EqualTo("-9.0E5"));

        result = subject.Parse(new ScanStrings("+9.0E-5"), null);
        Assert.That(result.Value, Is.EqualTo("+9.0E-5"));

        result = subject.Parse(new ScanStrings("123__000_000.0234e222"), null);
        Assert.That(result.Value, Is.EqualTo("123__000_000.0234e222"));

        result = subject.Parse(new ScanStrings("1_._0"), null);
        Assert.That(result.Value, Is.EqualTo("1_._0"));

        result = subject.Parse(new ScanStrings(".1"), null);
        Assert.That(result.Value, Is.EqualTo(".1"));

        result = subject.Parse(new ScanStrings(".1e-4"), null);
        Assert.That(result.Value, Is.EqualTo(".1e-4"));

        result = subject.Parse(new ScanStrings("1.e4"), null);
        Assert.That(result.Value, Is.EqualTo("1.e4"));

        result = subject.Parse(new ScanStrings("1."), null);
        Assert.That(result.Value, Is.EqualTo("1."));

        // Invalid cases

        result = subject.Parse(new ScanStrings("  1.0"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("."), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("+"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings(".e4"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("-"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("++1"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("-+1"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("_1.0"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("1.0_"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("1..0"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("1._.0"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("1.0e1.0"), null);
        Assert.That(result.Success, Is.False);
    }

    [Test]
    public void _VariableWidthFractionalDecimal_with_leading_zero()
    {
        var subject = new VariableWidthFractionalDecimal(groupMark: "_", decimalMark: ".",
            allowLeadingWhitespace: false, allowLoneDecimal: true, allowLeadingZero: true, allowLeadingPlus: true);
        Console.WriteLine(subject.ToString());

        var result = subject.Parse(new ScanStrings("0234"), null);
        Assert.That(result.Value, Is.EqualTo("0234"));

        result = subject.Parse(new ScanStrings("0000"), null);
        Assert.That(result.Value, Is.EqualTo("0000"));

        result = subject.Parse(new ScanStrings("0"), null);
        Assert.That(result.Value, Is.EqualTo("0"));

        result = subject.Parse(new ScanStrings("+0234"), null);
        Assert.That(result.Value, Is.EqualTo("+0234"));

        result = subject.Parse(new ScanStrings("-0234"), null);
        Assert.That(result.Value, Is.EqualTo("-0234"));

        result = subject.Parse(new ScanStrings("1.0"), null);
        Assert.That(result.Value, Is.EqualTo("1.0"));

        result = subject.Parse(new ScanStrings("0.1"), null);
        Assert.That(result.Value, Is.EqualTo("0.1"));

        result = subject.Parse(new ScanStrings("00.1"), null);
        Assert.That(result.Value, Is.EqualTo("00.1"));

        result = subject.Parse(new ScanStrings("+99"), null);
        Assert.That(result.Value, Is.EqualTo("+99"));

        result = subject.Parse(new ScanStrings("-99"), null);
        Assert.That(result.Value, Is.EqualTo("-99"));

        result = subject.Parse(new ScanStrings("-9.0E5"), null);
        Assert.That(result.Value, Is.EqualTo("-9.0E5"));

        result = subject.Parse(new ScanStrings("+9.0E-5"), null);
        Assert.That(result.Value, Is.EqualTo("+9.0E-5"));

        result = subject.Parse(new ScanStrings("123__000_000.0234e222"), null);
        Assert.That(result.Value, Is.EqualTo("123__000_000.0234e222"));

        result = subject.Parse(new ScanStrings("1_._0"), null);
        Assert.That(result.Value, Is.EqualTo("1_._0"));

        result = subject.Parse(new ScanStrings(".1"), null);
        Assert.That(result.Value, Is.EqualTo(".1"));

        result = subject.Parse(new ScanStrings(".1e-4"), null);
        Assert.That(result.Value, Is.EqualTo(".1e-4"));

        result = subject.Parse(new ScanStrings("1."), null);
        Assert.That(result.Value, Is.EqualTo("1."));

        // Invalid cases

        result = subject.Parse(new ScanStrings("  1.0"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("."), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("+"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("-"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("++1"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("-+1"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("_1.0"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("1.0_"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("1..0"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("1._.0"), null);
        Assert.That(result.Success, Is.False);

        result = subject.Parse(new ScanStrings("1.0e1.0"), null);
        Assert.That(result.Success, Is.False);
    }

    [Test]
    public void _VariableWidthFractionalDecimal_with_leading_whitespace()
    {
        var subject = new VariableWidthFractionalDecimal(groupMark: "_", decimalMark: ".",
            allowLeadingWhitespace: true, allowLoneDecimal: false, allowLeadingZero: false, allowLeadingPlus: true);
        Console.WriteLine(subject.ToString());

        var result = subject.Parse(new ScanStrings("   234"), null);
        Assert.That(result.Value, Is.EqualTo("   234"));

        result = subject.Parse(new ScanStrings("  1.0"), null);
        Assert.That(result.Value, Is.EqualTo("  1.0"));
    }

    [Test]
    public void _VariableWidthFractionalDecimal_no_grouping()
    {
        var subject = new VariableWidthFractionalDecimal(groupMark: "", decimalMark: ".",
            allowLeadingWhitespace: true, allowLoneDecimal: false, allowLeadingZero: false, allowLeadingPlus: true);
        Console.WriteLine(subject.ToString());

        var result = subject.Parse(new ScanStrings("   234"), null);
        Assert.That(result.Value, Is.EqualTo("   234"));

        result = subject.Parse(new ScanStrings("  1.0"), null);
        Assert.That(result.Value, Is.EqualTo("  1.0"));
    }

    [Test]
    public void _VariableWidthFractionalDecimal_with_multi_char_marks()
    {
        var subject = new VariableWidthFractionalDecimal(groupMark: "group", decimalMark: "point",
            allowLeadingWhitespace: false, allowLoneDecimal: false, allowLeadingZero: false, allowLeadingPlus: true);
        Console.WriteLine(subject.ToString());

        var result = subject.Parse(new ScanStrings("1group000point5"), null);
        Assert.That(result.Value, Is.EqualTo("1group000point5"));

        result = subject.Parse(new ScanStrings("1point0"), null);
        Assert.That(result.Value, Is.EqualTo("1point0"));

        result = subject.Parse(new ScanStrings("1"), null);
        Assert.That(result.Value, Is.EqualTo("1"));

        result = subject.Parse(new ScanStrings("0"), null);
        Assert.That(result.Value, Is.EqualTo("0"));
    }

    [Test]
    public void _VariableWidthFractionalDecimal_ending_characters()
    {
        var subject = new VariableWidthFractionalDecimal(groupMark: "_", decimalMark: ".",
            allowLeadingWhitespace: true, allowLoneDecimal: false, allowLeadingZero: false, allowLeadingPlus: true);
        Console.WriteLine(subject.ToString());

        var result = subject.Parse(new ScanStrings("+123+123"), null);
        Assert.That(result.Value, Is.EqualTo("+123"));

        result = subject.Parse(new ScanStrings("1+1"), null);
        Assert.That(result.Value, Is.EqualTo("1"));

        result = subject.Parse(new ScanStrings("1.0-1"), null);
        Assert.That(result.Value, Is.EqualTo("1.0"));

        result = subject.Parse(new ScanStrings("1.0e4)"), null);
        Assert.That(result.Value, Is.EqualTo("1.0e4"));

        result = subject.Parse(new ScanStrings("1.0e4+4)"), null);
        Assert.That(result.Value, Is.EqualTo("1.0e4"));
    }

    [Test]
    public void _RangeExcludingCharacterSet_()
    {
        var nullParser = new NullParser("test");
        var subject    = new RangeExcludingCharacterSet('a', 'z', 'q', 'p', 'b', 'd');
        var input      = @"abcDEFghijklMNOpqrstuvwxyz";
        var expected   = @"acghijklrstuvwxyz";
        var scanner    = new ScanStrings(input);

        var result = new StringBuilder();

        var offset = 0;
        for (var index = 0; index < input.Length; index++)
        {
            var c     = input[index];
            var match = subject.Parse(scanner, scanner.CreateMatch(nullParser, offset, index, null));
            if (match.Success) result.Append(c);
        }

        Console.WriteLine(result.ToString());
        Assert.That(result.ToString(), Is.EqualTo(expected));
    }

    [Test]
    public void _MultiRangeCharacterSet_()
    {
        var nullParser = new NullParser("test");
        var subject    = BNF.CharacterInRanges(('a', 'g'), ('A', 'G'), 'z');
        var input      = @"abcDEFghijklMNOpqrstuvwxyz";
        var expected   = @"abcDEFgz";
        var scanner    = new ScanStrings(input);

        var result = new StringBuilder();

        var offset = 0;
        for (var index = 0; index < input.Length; index++)
        {
            var c     = input[index];
            var match = subject.Parse(scanner, scanner.CreateMatch(nullParser, offset, index, null));
            if (match.Success) result.Append(c);
        }

        Console.WriteLine(result.ToString());
        Assert.That(result.ToString(), Is.EqualTo(expected));
    }

    [Test]
    public void _MultiRangeExcludingCharacterSet_()
    {
        var nullParser = new NullParser("test");
        var subject    = BNF.CharacterNotInRanges(('a', 'g'), ('A', 'G'), 'z');
        var input      = @"abcDEFghijklMNOpqrstuvwxyz";
        var expected   = @"hijklMNOpqrstuvwxy";
        var scanner    = new ScanStrings(input);

        var result = new StringBuilder();

        var offset = 0;
        for (var index = 0; index < input.Length; index++)
        {
            var c     = input[index];
            var match = subject.Parse(scanner, scanner.CreateMatch(nullParser, offset, index, null));
            if (match.Success) result.Append(c);
        }

        Console.WriteLine(result.ToString());
        Assert.That(result.ToString(), Is.EqualTo(expected));
    }


    [Test]
    public void fancy_quotes()
    {
        var subject = BNF.LeftSingleQuote > "A " > BNF.LeftDoubleQuote > "quote" > BNF.RightDoubleQuote > " for you" > BNF.RightSingleQuote;
        var input   = "‘A “quote” for you’";
        var scanner = new ScanStrings(input);

        var result  = subject.Parse(scanner);
        Assert.That(result.Success, Is.True);
    }
}