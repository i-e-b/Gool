﻿using System;
using Gool.Results;

namespace Gool.Scanners;

/// <summary>
/// Dumb stack implementation for garbage minimisation.
/// This is on a very hot path, so is a bit weird.
/// </summary>
internal class MatchPool
{
    /// <summary> Array full of pooled parser matches </summary>
    private readonly ParserMatch[] _buffer;

    /// <summary> Capacity of the buffer </summary>
    private readonly int _capacity;

    /// <summary> Last accepted push </summary>
    private ParserMatch _compare = _nothing;

    /// <summary> Number of items we are allowed to pop </summary>
    private int _available;

    /// <summary> Index of the first element in buffer </summary>
    private int _start;

    /// <summary> Index after the last element in the buffer </summary>
    private int _end;

    /// <summary> Number of items stored </summary>
    private int _size;

    /// <summary> Placeholder parser match </summary>
    private static readonly ParserMatch _nothing = new();

    /// <summary>
    /// Initializes a new match pool with a fixed size
    /// </summary>
    public MatchPool(int capacity)
    {
        _capacity = capacity;
        _buffer = new ParserMatch[capacity];
        _size = 0;
        _start = 0;
        _end = 0;
    }

    /// <summary>
    /// Remove and return an item, if there are any
    /// </summary>
    public bool TryPop(out ParserMatch item)
    {
        if (_available < 1)
        {
            item = _nothing;
            return false;
        }

        var result = _buffer[_start++];
        if (_start == _capacity) _start = 0;
        _size--;
        item = result;
        _available--;
        _compare = _available > 0 ? _buffer[_start] : _nothing;
        return true;
    }

    /// <summary>
    /// Push a new item only if it doesn't match the current top item
    /// </summary>
    public void PushNoMatch(ParserMatch item)
    {
        if (_size >= _capacity) return; // full
        if (ReferenceEquals(item, _compare)) return; // duplicate

        _compare = item;
        _buffer[_end++] = item;
        if (_end == _capacity) _end = 0;
        _size++;
    }

    /// <summary>
    /// Reset the stack to empty state
    /// </summary>
    public void Clear()
    {
        // to clear we just reset everything.
        _start = 0;
        _end = 0;
        _size = 0;
        Array.Clear(_buffer, 0, _buffer.Length);
        _available = 0;
    }

    /// <summary>
    /// Move the read head up to the write head
    /// </summary>
    public void Absorb()
    {
        _available = _size;
    }
}