using Phantom;
// ReSharper disable InconsistentNaming
// ReSharper disable MemberCanBePrivate.Global

namespace Samples;

/// <summary>
/// Derived from https://www.rfc-editor.org/rfc/rfc3339.txt
/// </summary>
public static class DateTimeExamples
{
    /// <summary>
    /// A complete parser for ISO8601, including deprecated truncated forms.
    /// <p/>
    /// This is a very complex format. Compare to <see cref="Rfc3339"/>
    /// and even the syntax of a full programming language: <see cref="PascalExample.Parser"/>
    /// </summary>
    public static BNF.Package Iso8601()
    {
        // Basic parts
        BNF hyphen        = '-';
        BNF opt_colon     = BNF.Optional(':');
        BNF decimal_point = BNF.OneOf(',', '.');
        BNF plus_minus    = BNF.OneOf('+', '-');
        BNF digits        = BNF.Regex("[0-9]+");
        BNF time_marker   = BNF.OneOf('T', 't', ' ');
        BNF zulu_marker   = BNF.OneOf('Z', 'z'); // UTC time

        // Date
        BNF date_century    = BNF.FixedDec(min: 0, max: 99, width: 2); // 00-99
        BNF date_decade     = BNF.FixedDec(min: 0, max: 9, width: 1);  // 0-9
        BNF date_sub_decade = BNF.FixedDec(min: 0, max: 9, width: 1);  // 0-9
        BNF date_year       = date_decade > date_sub_decade;
        BNF date_month      = BNF.FixedDec(min: 1, max: 12, width: 2);  // 01-12
        BNF date_w_day      = BNF.FixedDec(min: 1, max: 7, width: 1);   // 1-7
        BNF date_m_day      = BNF.FixedDec(min: 1, max: 31, width: 2);  // 01-31
        BNF date_y_day      = BNF.FixedDec(min: 1, max: 366, width: 3); // 001-365
        BNF date_week       = BNF.FixedDec(min: 1, max: 53, width: 2);  // 01-53
        BNF date_year_part  = (!date_century) > (date_year);

        BNF datePart_fullYear =  date_year_part > (!hyphen);
        BNF datePart_ptYear   = '-' > !(date_sub_decade > (!hyphen));
        BNF datePart_wkYear   = datePart_ptYear | datePart_fullYear;

        BNF dateOpt_century  = '-' | date_century;
        BNF dateOpt_fullYear = '-' | datePart_fullYear;
        BNF dateOpt_year     = '-' | (date_year > !hyphen);
        BNF dateOpt_month    = '-' | (date_month > !hyphen);
        BNF dateOpt_week     = '-' | (date_week > !hyphen);

        BNF dateSpec_full   = datePart_fullYear > date_month > (!hyphen) > date_m_day;
        BNF dateSpec_year   = date_century | (dateOpt_century > date_year);
        BNF dateSpec_month  = '-' > dateOpt_year > date_month > !(!hyphen > date_m_day);
        BNF dateSpec_m_day  = "--" > dateOpt_month > date_m_day;
        BNF dateSpec_week   = datePart_wkYear > 'W' > (date_week | (dateOpt_week > date_w_day));
        BNF dateSpec_w_day  = "---" > date_w_day;
        BNF dateSpec_y_day  = dateOpt_fullYear > date_y_day;

        BNF date = dateSpec_full | dateSpec_year | dateSpec_month | dateSpec_m_day | dateSpec_week | dateSpec_w_day | dateSpec_y_day;
        
        // Time
        BNF time_hour        = BNF.FixedDec(min: 0, max: 24, width: 2); // 00-24
        BNF time_minute      = BNF.FixedDec(min: 0, max: 59, width: 2); // 00-59
        BNF time_second      = BNF.FixedDec(min: 0, max: 60, width: 2); // 00-59, or 60 if leap-second
        BNF time_fraction    = decimal_point > digits; // .1234
        BNF time_offset_num  = plus_minus > time_hour > !(opt_colon > time_minute);
        BNF time_zone        = zulu_marker > (!time_offset_num).Tagged(TimeZone);

        BNF timeOpt_hour     = hyphen | (time_hour > opt_colon);
        BNF timeOpt_minute   = hyphen | (time_minute > opt_colon);

        BNF timeSpec_hour    = time_hour > !(opt_colon > time_minute > !(opt_colon > time_second));
        BNF timeSpec_minute  = timeOpt_hour > time_minute > !(opt_colon > time_second);
        BNF timeSpec_second  = hyphen > timeOpt_minute > time_second;
        BNF timeSpec_base    = timeSpec_hour | timeSpec_minute | timeSpec_second;

        BNF time = timeSpec_base > !time_fraction > !time_zone;

        BNF iso_date_time = (date > !(!time_marker > time)) | (time);

        // Duration
        BNF dur_second     = digits.Tagged(Seconds)  > 'S';
        BNF dur_minute     = digits.Tagged(Minutes)  > 'M' > !dur_second;
        BNF dur_hour       = digits.Tagged(Hours)    > 'H' > !dur_minute;
        BNF dur_time       =                           'T' > (dur_hour | dur_minute | dur_second);
        BNF dur_day        = digits.Tagged(Days)     > 'D';
        BNF dur_week       = digits.Tagged(Weeks)    > 'W';
        BNF dur_month      = digits.Tagged(Months)   > 'M' > !dur_day;
        BNF dur_year       = digits.Tagged(Years)    > 'Y' > !dur_month;
        BNF dur_date       = (dur_day | dur_month | dur_year) > !dur_time;
        
        BNF duration =  'P' > ( dur_date | dur_time | dur_week);

        // Periods
        BNF p_mark = '/';
        BNF period_explicit = iso_date_time > p_mark > iso_date_time;
        BNF period_start    = iso_date_time > p_mark > duration;
        BNF period_end      = duration      > p_mark > iso_date_time;

        BNF period = period_explicit | period_start | period_end;

        // Totality
        BNF ISO8691 = iso_date_time | period | duration;
        
        
        // Tagging
        time_hour.Tag(Hour);
        time_minute.Tag(Minute);
        time_second.Tag(Second);
        time_fraction.Tag(SecondFraction);
        BNF.TagAll(Year, date_year_part, dateSpec_year);
        date_month.Tag(Month);
        date_m_day.Tag(DayOfMonth);
        date_w_day.Tag(DayOfWeek);
        date_y_day.Tag(DayOfYear);
        p_mark.Tag(PeriodMarker);
        

        return ISO8691.WithOptions(BNF.Options.None);
    }

    /// <summary>
    /// A complete parser for RFC3339
    /// <p/>
    /// This is a vastly simplified version of <see cref="Iso8601"/>,
    /// And is really what most people mean when they say 'ISO 8601'
    /// </summary>
    public static BNF.Package Rfc3339()
    {
        BNF decimal_point = BNF.OneOf(',', '.');
        BNF plus_minus    = BNF.OneOf('+', '-');
        BNF digits        = BNF.Regex("[0-9]+");
        BNF time_marker   = BNF.OneOf('T', 't', ' ');
        BNF zulu_marker   = BNF.OneOf('Z', 'z'); // UTC time
        
        BNF date_fullYear = BNF.FixedDec(min: 0, max: 9999, width: 4); // 0000-9999
        BNF date_month    = BNF.FixedDec(min: 1, max: 12, width: 2); // 01-12
        BNF date_m_day    = BNF.FixedDec(min: 1, max: 31, width: 2); // 01-31
        
        BNF time_hour     = BNF.FixedDec(min: 0, max: 23, width: 2); // 00-23
        BNF time_minute   = BNF.FixedDec(min: 0, max: 59, width: 2); // 00-59
        BNF time_second   = BNF.FixedDec(min: 0, max: 60, width: 2); // 00-60

        BNF time_secFrac   = decimal_point > digits;
        BNF time_numOffset = plus_minus > time_hour > ':' > time_minute;
        BNF time_offset    = zulu_marker | time_numOffset;

        BNF partial_time   = time_hour > ':' > time_minute > ':' > time_second > !time_secFrac;

        BNF full_date      = date_fullYear > '-' > date_month > '-' > date_m_day;
        BNF full_time      = partial_time > time_offset;

        BNF date_time = full_date > time_marker > full_time;


        return date_time.WithOptions(BNF.Options.None);
    }

    // Period tags
    public const string PeriodMarker = "PeriodMarker";

    // Date tags
    public const string Year           = "Year";
    public const string Month          = "Month";
    public const string DayOfMonth     = "DayOfMonth";
    public const string DayOfWeek      = "DayOfWeek";
    public const string DayOfYear      = "DayOfYear";
    
    // Time tags
    public const string TimeZone       = "TimeZone";
    public const string Hour           = "Hour";
    public const string Minute         = "Minute";
    public const string Second         = "Second";
    public const string SecondFraction = "SecondFraction";
    
    // Duration tags
    public const string Seconds = "Seconds";
    public const string Minutes = "Minutes";
    public const string Hours   = "Hours";
    public const string Days    = "Days";
    public const string Weeks   = "Weeks";
    public const string Months  = "Months";
    public const string Years   = "Years";
}