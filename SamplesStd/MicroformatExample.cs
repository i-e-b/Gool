using System.Diagnostics.CodeAnalysis;
using Gool;
using static Gool.BNF;

namespace Samples;

[SuppressMessage("ReSharper", "AutoPropertyCanBeMadeGetOnly.Global")]
public class MicroformatExample
{
    public int Width { get; set; } = 320;
    public int Height { get; set; } = 240;

    public string Source { get; set; } = "Unknown";
    public ImageFormat Format { get; set; } = ImageFormat.Unknown;

    /// <summary>
    /// Parse the structure from a micro-format string
    /// </summary>
    public static MicroformatExample Parse(string src)
    {
        var dst = new MicroformatExample();
        Parser().ParseEntireString(src).TagsToProperties(dst);
        return dst;
    }

    /// <summary>
    /// Example of a small string format. Expects something like <c>camera:640,480;Rgba</c>
    /// </summary>
    private static ParserPackage Parser()
    {
        BNF
            size = Integer()[nameof(Width)] > ',' > Integer()[nameof(Height)],
            pattern = StringToEndOrTerminatedBy(":")[nameof(Source)] > ":" > size > ";" > RestOfInput[nameof(Format)];

        return pattern.Build();
    }
}

[SuppressMessage("ReSharper", "UnusedMember.Global")]
public enum ImageFormat
{
    Unknown, Rgba, Argb, Bgra, Abgr
}