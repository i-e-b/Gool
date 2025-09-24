using System;
using System.Collections.Generic;
using Gool.Parsers;
using Gool.Results;

namespace Gool.Scanners;

/// <summary>
/// Scanner that operates over strings.
/// </summary>
public class ScanStrings : IScanner
{
    private bool    _completed;
    private string? _transformedString;
    private string? _furthestTag;

    private readonly string                      _input;
    private readonly bool                        _recordDiagnostics;
    private readonly Dictionary<object, object?> _contexts      = new();
    private readonly HashSet<string>             _failedTags    = new();
    private readonly int                         _inputLength;

    private readonly MatchPool         _matchPool; // match pool, used to minimise dead match objects
    private readonly List<ParserMatch> _failedMatches = new(); // failed matches, for diagnostics

    /// <summary>
    /// Create a new scanner from an input string.
    /// </summary>
    /// <param name="input">String to scan</param>
    /// <param name="recordDiagnostics">If true, record extra details for failure diagnosis</param>
    public ScanStrings(string input, bool recordDiagnostics = true)
    {
        _input = input;
        _recordDiagnostics = recordDiagnostics;
        _inputLength = input.Length;
        FurthestOffset = 0;
        _completed = false;

        var poolSize = Math.Max(16, _inputLength / 256);
        _matchPool = new MatchPool(poolSize, this);

        Transform = new NoTransform();
    }

    /// <summary>
    /// If <c>true</c>, auto-advanced elements (like white-space skips)
    /// will be added to the result tree.
    /// </summary>
    public bool IncludeSkippedElements { get; set; }

    /// <summary>
    /// Get the original input string
    /// </summary>
    public string InputString => _input;

    /// <summary>
    /// The input string, as processed by the transformer.
    /// This will be equal to the input string if there is no transformer.
    /// </summary>
    public string TransformedString
    {
        get
        {
            _transformedString ??= Transform.Transform(_input);
            return _transformedString;
        }
    }

    #region IScanner Members

    /// <inheritdoc />
    public void Complete()
    {
        _completed = true;

        // Remove links to reduce GC tracing
        foreach (var match in _failedMatches) { match.Reset(); }
        _failedMatches.Clear();
        _matchPool.Clear();
    }

    /// <inheritdoc />
    public void SetContext(IParser parser, object? context)
    {
        _contexts[parser] = context;
    }

    /// <inheritdoc />
    public object? GetContext(IParser parser)
    {
        return _contexts.GetValueOrDefault(parser);
    }

    /// <inheritdoc />
    public List<string> ListFailures(int minimumOffset = 0, bool showDetails = false)
    {
        var lst = new List<string>();

        if (FurthestTest is not null)
        {
            if (_failedTags.Count > 0)
            {
                lst.Add($"Expected '{string.Join("', '", _failedTags)}'");
            }

            if (_furthestTag is not null) lst.Add($"After '{_furthestTag}'");

            var offset = FurthestMatch?.Offset ?? 0;
            var prev   = UntransformedSubstring(0, offset);
            var length = Math.Max(0, (FurthestTest?.Right ?? _input.Length) - offset);
            var left   = UntransformedSubstring(offset, length);
            var right  = UntransformedSubstring(offset + length, _input.Length);
            lst.Add($"{prev}◢{left}◣{right}");
        }

        if (!showDetails) return lst;


        foreach (var p in _failedMatches)
        {
            if (p.Offset < minimumOffset) continue;

            var prev  = _input[..p.Offset];
            var left  = p.Length >= 0 ? _input.Substring(p.Offset, p.Length) : "";
            var right = _input[(p.Offset + p.Length)..];

            lst.Add($"{prev}◢{left}◣{right} --> ({FurthestMatch?.Offset ?? 0},{FurthestMatch?.Right ?? 0}..{FurthestTest?.Offset ?? 0},{FurthestTest?.Right ?? 0}) {ParserStringFrag(p)}");
        }

        return lst;
    }

    private static string ParserStringFrag(ParserMatch p)
    {
        if (!string.IsNullOrWhiteSpace(p.Tag)) return p.Tag;
        var str = p.SourceParser.ShortDescription(depth: 7);
        return str;
    }

    /// <inheritdoc />
    public bool EndOfInput(int offset)
    {
        return offset >= _inputLength;
    }

    /// <inheritdoc />
    public bool Read(ref int offset)
    {
        if (_completed) throw new Exception("This scanner has been completed");
        if (EndOfInput(offset)) return false;

        offset++;

        return !EndOfInput(offset);
    }

    /// <inheritdoc />
    public char Peek(int offset)
    {
        if (offset >= _inputLength) return (char)0;
        return TransformedString[offset];
    }

    /// <summary>
    /// If skip whitespace is set and current position is whitespace,
    /// seek forward until on non-whitespace position or EOF.
    /// </summary>
    public ParserMatch? DoAutoAdvance(ParserMatch? previous)
    {
        // It's very important to have auto-advance off!
        if (AutoAdvance is null) return previous;

        var left = previous?.Right ?? 0;
        var prev = NoMatch(AutoAdvance, previous);
        if (EndOfInput(left)) return prev;

        var skipMatch = AutoAdvance.Parse(this, prev, allowAutoAdvance: false);
        return (skipMatch.Length > 0) ? skipMatch : prev;
    }

    /// <inheritdoc />
    public ReadOnlySpan<char> Substring(int offset, int length)
    {
        return TransformedString.AsSpan(offset, Math.Min(length, _inputLength - offset));
    }

    /// <inheritdoc />
    public int IndexOf(int offset, string toFind, StringComparison comparisonType)
    {
        return TransformedString.IndexOf(toFind, offset, comparisonType);
    }

    /// <inheritdoc />
    public string UntransformedSubstring(int offset, int length)
    {
        if (length == 0) return "";
        if (length > 0) return InputString.Substring(offset, Math.Min(length, _inputLength - offset));

        // Caller has asked for negative length, which we handle as moving the offset back
        var left = Math.Max(0, offset + length);
        length = offset - left;
        return InputString.Substring(left, length);
    }

    /// <inheritdoc />
    public ITransform Transform { get; set; }

    /// <inheritdoc />
    public IParser? AutoAdvance { get; set; }

    /// <inheritdoc />
    public int FurthestOffset { get; private set; }

    /// <inheritdoc />
    public ParserMatch? FurthestMatch { get; private set; }

    /// <inheritdoc />
    public ParserMatch? FurthestTest { get; private set; }

    /// <inheritdoc />
    public string? LastTag { get; set; }

    /// <summary>
    /// Add a success path, for diagnostic use
    /// </summary>
    public void AddSuccess(ParserMatch newMatch)
    {
        _matchPool.Absorb(); // All failures can now be used

        if (!_recordDiagnostics) return;

        if (newMatch.Right > (FurthestMatch?.Right ?? 0)) FurthestMatch = newMatch;
        _furthestTag = LastTag;
        _failedTags.Clear();
        _failedMatches.Clear();
    }

    /// <inheritdoc />
    public void AddFailure(ParserMatch failMatch)
    {
        _matchPool.PushNoMatch(failMatch);

        if (!_recordDiagnostics) return;

        if (failMatch.Right > (FurthestTest?.Right ?? 0)) FurthestTest = failMatch;
        if (LastTag is not null) _failedTags.Add(LastTag);
        _failedMatches.Add(failMatch); // store for later diagnostics
    }


    /// <summary>
    /// Tell the scanner an old parser match can be recycled
    /// </summary>
    public void Absorb(ParserMatch old)
    {
        _matchPool.PushNoMatch(old);
    }

    /// <inheritdoc />
    public ParserMatch NoMatch(IParser source, ParserMatch? previous) => CreateMatch(source, previous?.Right ?? 0, -1, previous);

    /// <inheritdoc />
    public ParserMatch EmptyMatch(IParser source, int offset,ParserMatch? previous) => CreateMatch(source, offset, 0, previous);

    /// <inheritdoc />
    public ParserMatch CreateMatch(IParser source, int offset, int length, ParserMatch? previous)
    {
        FurthestOffset = Math.Max(offset + length, FurthestOffset);
        return _matchPool.Get(source, offset, length, previous);
    }

    #endregion
}