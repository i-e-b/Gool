using Gool.Results;
using NUnit.Framework;
using TestLanguageImplementation.Interpreted;

namespace TestLanguageImplementation;

[TestFixture]
public class LanguageExampleTests
{
    /// <summary>
    /// This runs a toy interpreter over the <see cref="TreeNode{T}"/> and <see cref="ScopeNode{T}"/>
    /// types provided with the parsers.
    /// </summary>
    /// <param name="input">User input for the program</param>
    /// <param name="expected">Expected 'print()' output</param>
    [Test]
    [TestCase("Boris", "Hello. What is your name?\r\nGreetings, Boris!\r\n12345")]
    [TestCase("", "Hello. What is your name?\r\nAll right then, keep your secrets.\r\n12345")]
    public void running_a_program_in_an_interpreter(string input, string expected)
    {
        var prog = File.ReadAllText("Sample1.txt");

        var interp = new Interpreter(prog);

        interp.SendLine(input); // simulate user input

        int i = 0;
        while (i < 1000)
        {
            if (!interp.Step()) break;
            i++;
        }

        Assert.That(i, Is.LessThan(900), "Interpreter got stuck?");

        Assert.That(interp.GetOutput(), Is.EqualTo(expected));
    }
}