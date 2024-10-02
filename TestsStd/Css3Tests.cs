using System.Diagnostics;
using NUnit.Framework;
using Samples;
using TestsStd.Helpers;

namespace TestsStd;

[TestFixture]
public class Css3Tests
{
    [Test]
    [TestCase(BasicSample)]
    [TestCase(TestSample)]
    [TestCase(TinySample)]
    [TestCase(EscapeSample)]
    [TestCase(MediaSample)]
    public void can_parse_css_files(string sample)
    {
        var sw     = Stopwatch.StartNew();
        var parser = Css3Example.Css3_Antlr();
        sw.Stop();
        Console.WriteLine($"Creating parser took {sw.Time()}");

        sw.Restart();
        var result = parser.ParseEntireString(sample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Time()}");


        if (!result.Success)
        {
            Console.WriteLine("\r\n==[ Failures ]===============================================================================");

            Console.WriteLine(
                $"Reached right-offset={result.Scanner.FurthestOffset}: {result.Scanner.UntransformedSubstring(result.Scanner.FurthestOffset, -50)}◢◣{result.Scanner.UntransformedSubstring(result.Scanner.FurthestOffset, 50)}");
            Console.WriteLine(result.Scanner.FurthestMatch?.Description() ?? "<no match>");
            foreach (var fail in result.Scanner.ListFailures())
            {
                Console.WriteLine(fail);
            }

            Console.WriteLine("\r\n=================================================================================");

            Console.WriteLine("[[" + result.Value + "]]");
        }

        Console.WriteLine("\r\n=================================================================================");

        foreach (var blm in result.TaggedTokensDepthFirst())
        {
            Console.WriteLine($"{blm.Value.Trim()} [{blm.Tag}]");
        }

        Assert.That(result.Success, Is.True);
        Assert.That(result.Value, Is.EqualTo(sample));
    }

    private const string BasicSample =
        """
            /* Style for a "Candidate Recommendation Draft" */
        
            @import "base.css";
        
            body {
              background-image: url(logos/CRD.svg);
            }
        """;

    private const string TestSample =
        """
        .blokken {
        	position: relative;
        	background-color: #fff;
        	width: calc(100vw - 10rem);
        	min-height: calc(100vh - 10rem);
        	box-shadow: -0.5rem 0.5rem 1rem rgba(0,0,0,0.3);
        	margin: 5rem;
        	font-size: 2rem;
        	background-image: linear-gradient(to bottom, #fff calc(1em - 1px), #ccc calc(1em - 1px), #ccc 1em, #fff 1em);
        	background-position: 0% 1em;
        	background-size: 100% 1em;
        	background-repeat: repeat-y;
        }
        """;

    private const string TinySample =
        """
        test {
        	width: calc(100vw + 10rem);
        }
        """;

    private const string EscapeSample =
        """
        c\0066fde {
            \66\o\n\t-si\ze: 1em;
            \te\x\t-ali\g\n: left;
            \white-\s\pace: \p\re\-\w\ra\p;
        }
        """;

    private const string MediaSample =
        """
        /* Style for a "Candidate Recommendation Draft" */

        @import "base.css";

        @media screen and (max-width: 500px) {
            .q-drawer--left,.q-header {
                top:64px
            }
        
            .ewp-table {
                max-width: 100%;
                min-width: unset
            }
        
            #leaflet-map {
                min-height: 320px
            }
        
            .ewp-table {
                height: 100%
            }
        
            .ewp-component-sm {
                min-width: 100%
            }
        
            #asset-details-grid {
                grid-template-areas: "image" "details" "status";
                grid-template-columns: 100%
            }
        
            #asset-image-container {
                width: 100%
            }
        
            .custom-apex-legend {
                bottom: 0!important;
                height: 80px!important;
                left: 0!important;
                position: relative!important
            }
        
            .custom-apex-legend-item {
                display: inline;
                margin-right: 7px
            }
        
            .custom-apex-container {
                margin-right: 0
            }
        
            .sponsor-picker {
                width: 200px
            }
        
            .tertiary-menu-block {
                display: flex;
                flex-direction: column
            }
        
            .tertiary-menu {
                height: 55.33px;
                overflow: auto;
                width: auto
            }
        
            .table-responsive table {
                border: 0
            }
        
            .table-responsive table thead {
                clip: rect(0 0 0 0);
                border: none;
                height: 1px;
                margin: -1px;
                overflow: hidden;
                padding: 0;
                position: absolute;
                width: 1px
            }
        
            .table-responsive table tr {
                background-color: #f8f8f8;
                border: solid #ddd;
                border-width: 1px 1px 3px;
                display: block;
                margin-bottom: .625em;
                padding: .35em
            }
        
            .table-responsive table td {
                border-bottom: 1px solid #ddd;
                display: block;
                font-size: 8pt;
                height: auto;
                min-height: 32px;
                padding: 5px;
                text-align: left;
                white-space: normal
            }
        
            .table-responsive table td:before {
                background: unset;
                content: attr(data-label);
                float: left;
                font-weight: 700;
                text-transform: uppercase
            }
        
            .table-responsive table td:first-child {
                width: 100%
            }
        
            .table-responsive table td:last-child {
                border-bottom: 0
            }
        }
        """;
}