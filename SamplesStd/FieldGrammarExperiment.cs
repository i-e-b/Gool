using Gool;

namespace Samples;

/// <summary>
/// Experiment with different way to build grammar
/// </summary>
public class FieldGrammarExperiment
{
    private static BNF
        x = "!",
        y = "hello",
        z = y > "world" > x;

    // https://stackoverflow.com/questions/72121/how-can-i-get-the-name-of-a-variable-passed-into-a-function
    // IEB: TODO: This kind of pattern, and use reflection to give a name to the parts.
    // TODO: Add 'Name' as well as Tag. Name for diagnostics only.
    // how to do recursive defs / forwards with the class-and-field layout? Maybe as functions? Is there a nicer way to get names from vars?
}