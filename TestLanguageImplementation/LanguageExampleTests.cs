using System.Diagnostics;
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
    [TestCase("Boris", "Hello. What is your name?\r\nGreetings, Boris!\r\n12345\r\nDone!")]
    [TestCase("", "Hello. What is your name?\r\nAll right then, keep your secrets.\r\n12345\r\nDone!")]
    public void running_a_program_in_an_interpreter(string input, string expected)
    {
        var prog = File.ReadAllText("Sample1.txt");
        Console.WriteLine($"Starting at {Stopwatch.GetTimestamp()}");

        var sw = Stopwatch.StartNew();

        var interp = new Interpreter(prog);

        sw.Stop();
        Console.WriteLine($"Start-up and parsing took {sw.Time()}");

        interp.SendLine(input); // simulate user input

        sw.Restart();
        int i = 0;
        while (i < 1000)
        {
            if (!interp.Step()) break;
            i++;
        }

        sw.Stop();
        Console.WriteLine($"Running interpreter took {sw.Time()}");

        Assert.That(i, Is.LessThan(900), "Interpreter got stuck?");

        Assert.That(interp.GetOutput(), Is.EqualTo(expected));
    }
}