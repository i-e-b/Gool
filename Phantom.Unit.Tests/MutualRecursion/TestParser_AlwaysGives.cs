using System;
using Phantom.Parsers;
using Phantom.Parsers.Interfaces;

namespace Phantom.Unit.Tests.MutualRecursion
{
    // ReSharper disable once InconsistentNaming
    public class TestParser_AlwaysGives : IMatchingParser
    {
        private readonly Func<ParserMatch> _func;
        public IScanner LastParseScanner { get; private set; }
        public IScanner LastTryMatchScanner { get; private set; }

        public TestParser_AlwaysGives(Func<ParserMatch> func)
        {
            _func = func;
        }

        public ParserMatch Parse(IScanner scan)
        {
            LastParseScanner = scan;
            return _func();
        }

        public ParserMatch TryMatch(IScanner scan)
        {
            LastTryMatchScanner = scan;
            return _func();
        }
    }
}