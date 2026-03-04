using NUnit.Framework;
using Samples;

namespace TestsStd;

[TestFixture]
public class MicroformatTests
{
    [Test]
    public void basic_type_application()
    {
        var result = MicroformatExample.Parse("camera:640,480;rgba");

        Assert.That(result, Is.Not.Null);

        Assert.That(result.Format, Is.EqualTo(ImageFormat.Rgba));
        Assert.That(result.Height, Is.EqualTo(480));
        Assert.That(result.Width, Is.EqualTo(640));
        Assert.That(result.Source, Is.EqualTo("camera"));
    }
}