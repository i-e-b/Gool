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
    [TestCase(ComplexSample)]
    public void can_parse_css_files(string sample)
    {
        // IEB: TODO: Use the failures to improve parser failure output

        var sw     = Stopwatch.StartNew();
        var parser = Css3Example.Css3_Antlr();
        sw.Stop();
        Console.WriteLine($"Creating parser took {sw.Time()}");

        sw.Restart();
        var result = parser.ParseEntireString(sample);
        sw.Stop();
        Console.WriteLine($"Parsing took {sw.Time()}");

        Console.WriteLine("\r\n==[ Failures ]===============================================================================");

        foreach (var fail in result.Scanner.ListFailures(true))
        {
            Console.WriteLine(fail);
        }

        Console.WriteLine("\r\n=================================================================================");

        Console.WriteLine("[["+result.Value+"]]");

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
        test {
        	size: 1em
        }
        """;

    private const string ComplexSample =
        """
        /* Style for a "Candidate Recommendation Draft" */
    
        @import "base.css";
        
        .clearfix:after {
            clear: both;
            content: " ";
            display: block;
            font-size: 0;
            height: 0;
            visibility: hidden;
        }
        .clearfix { display: inline-block; }
        /* * html .clearfix { height: 1%; } */
        .clearfix { display: block; }
        
        body {
            background-image: url(logos/CRD.svg);
            animation: bounce 300ms linear 2s infinite alternate-reverse forwards normal;
        }
        
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
        	
        	&:before,
        	&:after {
        		content: "";
        		position: absolute;
        		top: 0;
        		left: 0;
        		width: 100%;
        		height: 100%;
        		background-color: #fff;
        		box-shadow: -0.5rem 0.5rem 1rem rgba(0,0,0,0.1);
        	}
        	
        	&:before {
        		transform: rotate(-2deg);
        		z-index: -1;
        	}
        	
        	&:after {
        		transform: rotate(2deg);
        		z-index: -2;
        	}
        }
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