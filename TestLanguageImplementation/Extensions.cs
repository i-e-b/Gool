using System.Diagnostics;

namespace TestLanguageImplementation;

public static class Extensions
{

    public static string Time(this Stopwatch sw)
    {
        var elapsed = sw.Elapsed;
        if (elapsed.TotalMicroseconds < 1) return $"{elapsed.TotalNanoseconds} ns";
        if (elapsed.TotalMicroseconds < 1000) return $"{elapsed.TotalMicroseconds} µs";
        if (elapsed.TotalMilliseconds < 1000) return $"{elapsed.TotalMilliseconds} ms";
        if (elapsed.TotalSeconds < 60) return $"{elapsed.TotalSeconds} s";
        return elapsed.ToString();
    }


    public static string Time(this Stopwatch sw, double count)
    {
        var elapsed = sw.Elapsed / count;
        if (elapsed.TotalMicroseconds < 1) return $"{elapsed.TotalNanoseconds} ns avg across {count:0}";
        if (elapsed.TotalMicroseconds < 1000) return $"{elapsed.TotalMicroseconds} µs avg across {count:0}";
        if (elapsed.TotalMilliseconds < 1000) return $"{elapsed.TotalMilliseconds} ms avg across {count:0}";
        if (elapsed.TotalSeconds < 60) return $"{elapsed.TotalSeconds} s avg across {count:0}";
        return $"{elapsed} avg across {count:0}";
    }

}