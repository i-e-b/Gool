using Gool;

// ReSharper disable InconsistentNaming

namespace Samples;

/// <summary>
/// Csv parser example, with parametric breaks.
/// Shows a simple way to vary parser based on settings.
/// <p/>
/// From https://www.rfc-editor.org/rfc/rfc4180
/// </summary>
public static class CsvExample
{
    public static ParserPackage Csv(bool hasHeader, string columnBreakStr, string rowBreakStr)
    {
        BNF column_break = columnBreakStr;       // Column break (comma in CSV, tab in TSV)
        BNF row_break = rowBreakStr;             // Row break (usually line break)

        BNF text = +( BNF.AnyChar / (row_break | column_break));     // Anything except the row and column breaks
        BNF quoted = BNF.Regex("\"([^\"]|\"\")*\"");                 // Anything except one double-quote
        
        BNF field = quoted | text;
        BNF name = quoted | text;

        BNF record = field % column_break;
        BNF header = name % column_break;
        
        BNF file;
        if (hasHeader) file = header > row_break > (record % row_break) > (!row_break);
        else file = (record % row_break) > (!row_break);

        field.TagWith(Field);
        name.TagWith(ColumnName);
        row_break.TagWith(NewRow);

        record.EncloseScope().TagWith(Row);     // 'EncloseScope' is like an Open and Close scope, from its start to end
        header.EncloseScope().TagWith(Header);  //       (like a tagged empty result at either end of the child match range)

        return file.Build();
    }
    

    public static string Cleanup(string src)
    {
        src = src.Trim();
        return src.StartsWith('"')
            ? src.Trim('"').Replace("\"\"", "\"")
            : src;
    }
    
    public const string ColumnName = "ColumnName";
    public const string Field = "Field";
    public const string NewRow = "RowBreak";
    public const string Row = "Row";
    public const string Header = "Header";
}