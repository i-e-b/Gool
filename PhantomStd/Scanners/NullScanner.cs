using System;
using System.Collections.Generic;
using Phantom.Results;

namespace Phantom.Scanners;

/// <summary>
/// Placeholder for an invalid scanner
/// </summary>
public class NullScanner : IScanner
{
    private const string ErrorMsg = "The scanner used is a place-holder. Please check you have passed in an input scanner.";
    
    /// <inheritdoc />
    public string? FurthestMatch() => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public void AddFailure(IParser failedParser, ParserMatch? previousMatch) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public List<string> ListFailures(bool includePartialMatches = false) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public void ClearFailures() => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public string BadPatch(int length) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public bool EndOfInput(int offset) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public ITransform Transform
    {
        get => throw new InvalidOperationException(ErrorMsg);
        set => throw new InvalidOperationException(ErrorMsg);
    }

    /// <inheritdoc />
    public bool IncludeSkippedElements
    {
        get => throw new InvalidOperationException(ErrorMsg);
        set => throw new InvalidOperationException(ErrorMsg);
    }

    /// <inheritdoc />
    public ParserMatch NoMatch(IParser? source, ParserMatch? previous) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public ParserMatch EmptyMatch(IParser source, int offset) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public ParserMatch NullMatch(IParser? source, int offset) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public bool Read(ref int offset) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public char Peek(int offset) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public ParserMatch AutoAdvance(ParserMatch? previous) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public string Substring(int offset, int length) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public string UntransformedSubstring(int offset, int length) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public string RemainingData(int offset) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public ParserMatch CreateMatch(IParser source, int offset, int length) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public void AddPath(ParserMatch newMatch) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public void Complete() => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public void SetContext(IParser parser, object? context) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public object? GetContext(IParser parser) => throw new InvalidOperationException(ErrorMsg);
}