using System;
using System.Collections.Generic;
using Phantom.Results;

namespace Phantom.Scanners;

/// <summary>
/// Placeholder for an invalid scanner
/// </summary>
public class NullScanner : IScanner
{
    /// <inheritdoc />
    public string? FurthestMatch() => throw new InvalidOperationException();

    /// <inheritdoc />
    public void AddFailure(IParser failedParser, ParserMatch? previousMatch) => throw new InvalidOperationException();

    /// <inheritdoc />
    public List<string> ListFailures(bool includePartialMatches = false) => throw new InvalidOperationException();

    /// <inheritdoc />
    public void ClearFailures() => throw new InvalidOperationException();

    /// <inheritdoc />
    public string BadPatch(int length) => throw new InvalidOperationException();

    /// <inheritdoc />
    public bool EndOfInput(int offset) => throw new InvalidOperationException();

    /// <inheritdoc />
    public ITransform Transform
    {
        get => throw new InvalidOperationException();
        set => throw new InvalidOperationException();
    }

    /// <inheritdoc />
    public ParserMatch NoMatch(IParser? source, ParserMatch? previous) => throw new InvalidOperationException();

    /// <inheritdoc />
    public ParserMatch EmptyMatch(IParser source, int offset) => throw new InvalidOperationException();

    /// <inheritdoc />
    public ParserMatch NullMatch(IParser? source, int offset) => throw new InvalidOperationException();

    /// <inheritdoc />
    public bool Read(ref int offset) => throw new InvalidOperationException();

    /// <inheritdoc />
    public char Peek(int offset) => throw new InvalidOperationException();

    /// <inheritdoc />
    public ParserMatch AutoAdvance(ParserMatch? previous) => throw new InvalidOperationException();

    /// <inheritdoc />
    public string Substring(int offset, int length) => throw new InvalidOperationException();

    /// <inheritdoc />
    public string UntransformedSubstring(int offset, int length) => throw new InvalidOperationException();

    /// <inheritdoc />
    public string RemainingData(int offset) => throw new InvalidOperationException();

    /// <inheritdoc />
    public ParserMatch CreateMatch(IParser source, int offset, int length) => throw new InvalidOperationException();

    /// <inheritdoc />
    public void AddPath(ParserMatch newMatch) => throw new InvalidOperationException();
}