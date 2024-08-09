using System;
using Phantom;
using Phantom.Parsers;
using Phantom.Parsers.Interfaces;
using Phantom.Results;

namespace Samples;

internal class Consoler : Parser, IMatchingParser
{
    private readonly IMatchingParser src;

    public Consoler(IMatchingParser src)
    {
        this.src = src;
    }

    public ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch)
    {
        var offset = previousMatch?.Offset ?? 0;
        Console.WriteLine(scan.RemainingData(offset));
        return src.TryMatch(scan, previousMatch);
    }

    public override string ShortDescription(int depth)
    {
        return src.ShortDescription(depth);
    }
}