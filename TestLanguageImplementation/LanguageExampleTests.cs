using NUnit.Framework;

namespace TestLanguageImplementation;

[TestFixture]
public class LanguageExampleTests
{
    [Test]
    public void running_a_program()
    {
        var prog = File.ReadAllText("Sample1.txt");

        var interp = new Interpreter(prog);

        interp.SendLine("Boris"); // simulate user input

        int i = 0;
        while (i < 1000)
        {
            if (!interp.Step()) break;
            i++;
        }

        Assert.That(i, Is.LessThan(900), "Interpreter got stuck?");

        var expected = "Hello. What is your name?\r\nGreetings, Boris!\r\n12345";

        Assert.That(interp.GetOutput(), Is.EqualTo(expected));
    }
}