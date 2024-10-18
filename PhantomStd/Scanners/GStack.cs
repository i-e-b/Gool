using System.Collections.Generic;
using Gool.Results;

namespace Gool.Scanners;

/// <summary>
/// Dumb stack implementation for garbage minimisation.
/// This is on a very hot path, so is a bit weird.
/// </summary>
internal class GStack
{
    private readonly string            _name;
    private readonly List<ParserMatch> _x = new(64); // The depth here depends on the parser tree, but rarely gets above 50

    private ParserMatch _z = _nothing; // last pushed
    private int         _p; // position
    private int         _m; // maximum

    private static readonly ParserMatch _nothing = new();

    public GStack(string name)
    {
        _name = name;
        _x.Add(_nothing); // fake item
    }

    /// <summary>
    /// Remove and return an item, if there are any
    /// </summary>
    public bool TryPop(out ParserMatch item)
    {
        item = _x[_p];
        if (_p > 0)
        {
            _p--;
            _z = _x[_p];
            return true;
        }

        return false;
    }

    /// <summary>
    /// Push a new item only if it doesn't match the current top item
    /// </summary>
    public void PushNoMatch(ParserMatch item)
    {
        if (ReferenceEquals(item, _z)) return;
        _p++;
        _z = item;
        if (_p > _m)
        {
            _m++;
            _x.Add(item);
        }
        else _x[_p] = item;
    }

    /// <summary>
    /// Reset the stack to empty state
    /// </summary>
    public void Clear()
    {
        _x.Clear();
        _x.Add(_nothing); // fake item
        _m = 0;
        _p = 0;
    }

    /// <summary>
    /// Copy all items from another stack to this one,
    /// and clear the other stack
    /// </summary>
    public void Absorb(GStack otherStack)
    {
        var xf = otherStack._x;
        for (var i = otherStack._p; i > 0; i--)
        {
            var item = xf[i];
            _p++;
            if (_p > _m)
            {
                _m++;
                _x.Add(item);
            }
            else _x[_p] = item;
        }

        _z = _x[_p];

        otherStack._p = 0; // Don't really clear it.
    }
}