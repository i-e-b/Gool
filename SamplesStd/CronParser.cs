using Gool;
using static Gool.BNF;

namespace Samples;

/// <summary>
/// Based on https://en.wikipedia.org/wiki/Cron#Cron_expression
/// </summary>
public static class CronParser
{
    /// <summary>
    /// Parser for cron expressions
    /// </summary>
    public static Package Build()
    {
        BNF // Forms
            all        = OneOf('*', '?'),
            list       = ',',
            range      = '-',
            every      = '/', // "*/5" in the minutes field indicates every 5 minutes
            nthDay     = '#', // "5#3" in the day-of-week field corresponds to the third Friday of every month
            month      = IntegerRange(1, 12) | OneOf("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
            dayOfWeek  = IntegerRange(0, 6) | OneOf("sun", "mon", "tue", "wed", "thu", "fri", "sat"),
            dayOfMonth = IntegerRange(1, 31),
            hours      = IntegerRange(0, 23),
            minutes    = IntegerRange(0, 59),
            weekNumber = IntegerRange(0, 5),
            year       = IntegerRange(1970, 2099) | FixedSizeInteger(0, 99, 2),
            timezone   = IdentifierString(allowUnderscore: false, allowHyphen: true) > !('/' > IdentifierString(allowUnderscore: false, allowHyphen: true)),
            macro      = OneOf("@yearly", "@annually", "@monthly", "@weekly", "@daily", "@midnight", "@hourly", "@reboot");

        macro.TagWith(Macro);
        timezone.TagWith(Timezone);

        BNF // Specials
            last = "l", // In the day-of-week field, "5L" or "FriL" means "the last Friday of the month".
            //             In the day-of-month field, "L" means the last day of the month.
            weekday = "w"; // "15W" is "the nearest weekday to the 15th of the month."

        BNF // Minute compounds
            minuteRange = (minutes > range > minutes),
            minuteSteps = (minutes | minuteRange | all) > every > minutes,
            minuteList  = (minutes | minuteRange) % list,
            minuteAny   = all.Copy(),
            minutePart  = minuteAny | minuteList | minuteSteps;

        minuteAny.TagWith(AnyMinute);
        minuteRange.TagWith(MinuteRange);
        minutes.TagWith(Minutes);
        minuteSteps.TagWith(MinuteSteps);

        BNF // Hour compounds
            hourRange = (hours > range > hours),
            hourSteps = (hours | hourRange | all) > every > hours,
            hourList  = (hours | hourRange) % list,
            hourAny   = all.Copy(),
            hourPart  = hourAny | hourList | hourSteps;

        hourAny.TagWith(AnyHour);
        hourRange.TagWith(HourRange);
        hours.TagWith(Hours);
        hourSteps.TagWith(HourSteps);

        BNF // Day-of-Month compounds
            dayOfMonthRange          = (dayOfMonth > range > dayOfMonth),
            dayOfMonthSteps          = (dayOfMonth | dayOfMonthRange | all) > every > dayOfMonth,
            dayOfMonthList           = (dayOfMonth | dayOfMonthRange) % list,
            dayOfMonthAny            = all.Copy(),
            dayOfMonthLast           = last,
            dayOfMonthNearestWeekDay = dayOfMonth > weekday,
            dayOfMonthPart           = dayOfMonthAny | dayOfMonthLast | dayOfMonthList | dayOfMonthSteps | dayOfMonthNearestWeekDay;

        dayOfMonthAny.TagWith(DayOfMonthAny);
        dayOfMonthLast.TagWith(DayOfMonthLast);
        dayOfMonthRange.TagWith(DayOfMonthRange);
        dayOfMonth.TagWith(DayOfMonth);
        dayOfMonthSteps.TagWith(DayOfMonthSteps);
        dayOfMonthNearestWeekDay.TagWith(DayOfMonthNearestWeekDay);

        BNF // Month compounds
            monthRange = (month > range > month),
            monthSteps = (month | monthRange | all) > every > IntegerRange(1, 12),
            monthList  = (month | monthRange) % list,
            monthAny   = all.Copy(),
            monthPart  = monthAny | monthList | monthSteps;

        monthAny.TagWith(MonthAny);
        monthRange.TagWith(MonthRange);
        month.TagWith(Month);
        monthSteps.TagWith(MonthSteps);

        BNF // Day-of-Week compounds
            dayOfWeekRange = (dayOfWeek > range > dayOfWeek),
            dayOfWeekSteps = (dayOfWeek | dayOfWeekRange | all) > every > IntegerRange(1, 7),
            dayOfWeekNth   = (dayOfWeek > nthDay > weekNumber),
            dayOfWeekList  = (dayOfWeek | dayOfWeekRange | dayOfWeekNth) % list,
            dayOfWeekAny   = all.Copy(),
            dayOfWeekLast  = dayOfWeek > last,
            dayOfWeekPart  = dayOfWeekAny | dayOfWeekList | dayOfWeekSteps | dayOfWeekLast;

        dayOfWeekAny.TagWith(DayOfWeekAny);
        dayOfWeekNth.TagWith(DayOfWeekNth);
        dayOfWeekRange.TagWith(DayOfWeekRange);
        dayOfWeek.TagWith(DayOfWeek);
        dayOfWeekSteps.TagWith(DayOfWeekSteps);
        dayOfWeekLast.TagWith(DayOfWeekLast);

        BNF // Year compounds
            yearRange = (year > range > year),
            yearList  = (year | yearRange) % list,
            yearAny   = all.Copy(),
            yearPart  = yearList | yearRange | yearAny;

        year.TagWith(Year);
        yearAny.TagWith(YearAny);
        yearRange.TagWith(YearRange);


        BNF
            cronFull = minutePart > hourPart > dayOfMonthPart > monthPart > dayOfWeekPart > !yearPart > !timezone,
            cron     = macro | cronFull;

        return cron.WithOptions(Options.IgnoreCase | Options.SkipWhitespace);
    }

    public const string Macro    = "Macro";
    public const string Timezone = "Timezone";

    public const string Year      = "Year";
    public const string YearAny   = "YearAny";
    public const string YearRange = "YearRange";

    public const string AnyMinute   = "AnyMinute";
    public const string MinuteRange = "MinuteRange";
    public const string Minutes     = "Minutes";
    public const string MinuteSteps = "MinuteSteps";

    public const string AnyHour   = "AnyHour";
    public const string HourRange = "HourRange";
    public const string Hours     = "Hours";
    public const string HourSteps = "HourSteps";

    public const string DayOfMonthAny            = "DayOfMonthAny";
    public const string DayOfMonthLast           = "DayOfMonthLast";
    public const string DayOfMonthRange          = "DayOfMonthRange";
    public const string DayOfMonth               = "DayOfMonth";
    public const string DayOfMonthSteps          = "DayOfMonthSteps";
    public const string DayOfMonthNearestWeekDay = "DayOfMonthNearestWeekDay";

    public const string MonthAny   = "MonthAny";
    public const string MonthRange = "MonthRange";
    public const string Month      = "Month";
    public const string MonthSteps = "MonthSteps";

    public const string DayOfWeekAny   = "DayOfWeekAny";
    public const string DayOfWeekNth   = "DayOfWeekNth";
    public const string DayOfWeekRange = "DayOfWeekRange";
    public const string DayOfWeek      = "DayOfWeek";
    public const string DayOfWeekSteps = "DayOfWeekSteps";
    public const string DayOfWeekLast  = "DayOfWeekLast";
}