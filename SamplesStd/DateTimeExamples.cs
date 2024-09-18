using Gool;
using static Gool.BNF;

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
    public static Package Iso8601()
    {
        BNF // Basic parts
            hyphen        = '-',
            opt_colon     = Optional(':'),
            decimal_point = OneOf(',', '.'),
            plus_minus    = OneOf('+', '-'),
            digits        = Regex("[0-9]+"),
            time_marker   = OneOf('T', 't', ' '),
            zulu_marker   = OneOf('Z', 'z'); // UTC time

        BNF // Date
            date_century    = FixedSizeInteger(min: 0, max: 99, width: 2), // 00-99
            date_decade     = FixedSizeInteger(min: 0, max: 9, width: 1), // 0-9
            date_sub_decade = FixedSizeInteger(min: 0, max: 9, width: 1), // 0-9
            date_year       = date_decade > date_sub_decade,
            date_month      = FixedSizeInteger(min: 1, max: 12, width: 2), // 01-12
            date_w_day      = FixedSizeInteger(min: 1, max: 7, width: 1), // 1-7
            date_m_day      = FixedSizeInteger(min: 1, max: 31, width: 2), // 01-31
            date_y_day      = FixedSizeInteger(min: 1, max: 366, width: 3), // 001-365
            date_week       = FixedSizeInteger(min: 1, max: 53, width: 2), // 01-53
            date_year_part  = (!date_century) > (date_year);

        BNF datePart_fullYear = date_year_part > (!hyphen),
            datePart_ptYear   = '-' > !(date_sub_decade > (!hyphen)),
            datePart_wkYear   = datePart_ptYear | datePart_fullYear;

        BNF dateOpt_century  = '-' | date_century,
            dateOpt_fullYear = '-' | datePart_fullYear,
            dateOpt_year     = '-' | (date_year > !hyphen),
            dateOpt_month    = '-' | (date_month > !hyphen),
            dateOpt_week     = '-' | (date_week > !hyphen);

        BNF dateSpec_full  = datePart_fullYear > date_month > (!hyphen) > date_m_day,
            dateSpec_year  = date_century | (dateOpt_century > date_year),
            dateSpec_month = '-' > dateOpt_year > date_month > !(!hyphen > date_m_day),
            dateSpec_m_day = "--" > dateOpt_month > date_m_day,
            dateSpec_week  = datePart_wkYear > 'W' > (date_week | (dateOpt_week > date_w_day)),
            dateSpec_w_day = "---" > date_w_day,
            dateSpec_y_day = dateOpt_fullYear > date_y_day;

        BNF // Time
            time_hour       = FixedSizeInteger(min: 0, max: 24, width: 2), // 00-24
            time_minute     = FixedSizeInteger(min: 0, max: 59, width: 2), // 00-59
            time_second     = FixedSizeInteger(min: 0, max: 60, width: 2), // 00-59, or 60 if leap-second
            time_fraction   = decimal_point > digits, // .1234
            time_offset_num = plus_minus > time_hour > !(opt_colon > time_minute),
            time_zone       = zulu_marker > (!time_offset_num).Tagged(TimeZone);

        BNF timeOpt_hour   = hyphen | (time_hour > opt_colon),
            timeOpt_minute = hyphen | (time_minute > opt_colon);

        BNF timeSpec_hour   = time_hour > !(opt_colon > time_minute > !(opt_colon > time_second)),
            timeSpec_minute = timeOpt_hour > time_minute > !(opt_colon > time_second),
            timeSpec_second = hyphen > timeOpt_minute > time_second,
            timeSpec_base   = timeSpec_hour | timeSpec_minute | timeSpec_second;

        BNF time = timeSpec_base > !time_fraction > !time_zone,
            date = dateSpec_full | dateSpec_year | dateSpec_month | dateSpec_m_day | dateSpec_week | dateSpec_w_day | dateSpec_y_day;

        // Complete date/time pattern
        BNF iso_date_time = (date > !(!time_marker > time)) | (time);

        // Duration
        BNF dur_second = digits.Tagged(Seconds) > 'S',
            dur_minute = digits.Tagged(Minutes) > 'M' > !dur_second,
            dur_hour   = digits.Tagged(Hours) > 'H' > !dur_minute,
            dur_time   = 'T' > (dur_hour | dur_minute | dur_second),
            dur_day    = digits.Tagged(Days) > 'D',
            dur_week   = digits.Tagged(Weeks) > 'W',
            dur_month  = digits.Tagged(Months) > 'M' > !dur_day,
            dur_year   = digits.Tagged(Years) > 'Y' > !dur_month,
            dur_date   = (dur_day | dur_month | dur_year) > !dur_time;

        BNF duration = 'P' > (dur_date | dur_time | dur_week);

        BNF // Periods
            p_mark          = '/',
            period_explicit = iso_date_time > p_mark > iso_date_time,
            period_start    = iso_date_time > p_mark > duration,
            period_end      = duration > p_mark > iso_date_time;

        BNF period = period_explicit | period_start | period_end;

        // Totality
        BNF ISO8691 = iso_date_time | period | duration;


        // Tagging
        time_hour.TagWith(Hour);
        time_minute.TagWith(Minute);
        time_second.TagWith(Second);
        time_fraction.TagWith(SecondFraction);

        date_year_part.TagWith(Year);
        dateSpec_year.TagWith(Year);
        date_month.TagWith(Month);
        date_m_day.TagWith(DayOfMonth);
        date_w_day.TagWith(DayOfWeek);
        date_y_day.TagWith(DayOfYear);
        p_mark.TagWith(PeriodMarker);

        return ISO8691.WithOptions(Options.None);
    }

    /// <summary>
    /// A complete parser for RFC3339
    /// <p/>
    /// This is a vastly simplified version of <see cref="Iso8601"/>,
    /// And is really what most people mean when they say 'ISO 8601'
    /// </summary>
    public static Package Rfc3339()
    {
        BNF // Fragments
            decimal_point = OneOf(',', '.'),
            plus_minus    = OneOf('+', '-'),
            digits        = Regex("[0-9]+"),
            time_marker   = OneOf('T', 't', ' '),
            zulu_marker   = OneOf('Z', 'z'); // UTC time

        BNF // Date
            date_fullYear = FixedSizeInteger(min: 0, max: 9999, width: 4), // 0000-9999
            date_month    = FixedSizeInteger(min: 1, max: 12, width: 2), // 01-12
            date_m_day    = FixedSizeInteger(min: 1, max: 31, width: 2); // 01-31

        BNF // Time
            time_hour   = FixedSizeInteger(min: 0, max: 23, width: 2), // 00-23
            time_minute = FixedSizeInteger(min: 0, max: 59, width: 2), // 00-59
            time_second = FixedSizeInteger(min: 0, max: 60, width: 2); // 00-60

        BNF // Sub-units
            time_secFrac   = decimal_point > digits,
            time_numOffset = plus_minus > time_hour > ':' > time_minute,
            time_offset    = zulu_marker | time_numOffset,
            partial_time   = time_hour > ':' > time_minute > ':' > time_second > !time_secFrac;

        BNF // Complete units
            full_date = date_fullYear > '-' > date_month > '-' > date_m_day,
            full_time = partial_time > time_offset,
            date_time = full_date > time_marker > full_time;

        return date_time.WithOptions(Options.None);
    }

    // Period tags
    public const string PeriodMarker = "PeriodMarker";

    // Date tags
    public const string Year       = "Year";
    public const string Month      = "Month";
    public const string DayOfMonth = "DayOfMonth";
    public const string DayOfWeek  = "DayOfWeek";
    public const string DayOfYear  = "DayOfYear";

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