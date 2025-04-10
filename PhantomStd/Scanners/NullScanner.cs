﻿using System;
using System.Collections.Generic;
using Gool.Parsers;
using Gool.Results;

namespace Gool.Scanners;

/// <summary>
/// Placeholder for an invalid scanner
/// </summary>
public class NullScanner : IScanner
{
    private const string ErrorMsg = "The scanner used is a place-holder. Please check you have passed in an input scanner.";

    /// <inheritdoc />
    public void AddFailure(ParserMatch failMatch) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public List<string> ListFailures(int minimumOffset = 0, bool showDetails = false) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public string InputString => throw new InvalidOperationException(ErrorMsg);
    /// <inheritdoc />
    public string TransformedString => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public bool EndOfInput(int offset) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public ITransform Transform
    {
        get => throw new InvalidOperationException(ErrorMsg);
        set => throw new InvalidOperationException(ErrorMsg);
    }

    /// <inheritdoc />
    public IParser? AutoAdvance { get; set; }

    /// <inheritdoc />
    public bool IncludeSkippedElements
    {
        get => throw new InvalidOperationException(ErrorMsg);
        set => throw new InvalidOperationException(ErrorMsg);
    }

    /// <inheritdoc />
    public int FurthestOffset => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public ParserMatch FurthestMatch => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public ParserMatch FurthestTest => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public string? LastTag { get; set; }

    /// <inheritdoc />
    public ParserMatch NoMatch(IParser? source, ParserMatch? previous) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public ParserMatch EmptyMatch(IParser source, int offset, ParserMatch? previous) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public ParserMatch NullMatch(IParser? source, int offset, ParserMatch? previous) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public ParserMatch CreateMatch(IParser source, int offset, int length, ParserMatch? previous) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public bool Read(ref int offset) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public char Peek(int offset) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public ParserMatch DoAutoAdvance(ParserMatch? previous) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public ReadOnlySpan<char> Substring(int offset, int length) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public string UntransformedSubstring(int offset, int length) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public void AddSuccess(ParserMatch newMatch) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public void Complete() => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public void SetContext(IParser parser, object? context) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public object GetContext(IParser parser) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public int IndexOf(int offset, string toFind, StringComparison comparisonType) => throw new InvalidOperationException(ErrorMsg);

    /// <inheritdoc />
    public void Absorb(ParserMatch old) => throw new InvalidOperationException(ErrorMsg);
}