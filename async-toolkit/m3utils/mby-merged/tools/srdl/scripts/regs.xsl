<?xml version="1.0" encoding="ISO-8859-1"?>
<!DOCTYPE xsl:stylesheet [ 
<!ENTITY nbsp "&#160;">   <!-- white space in XSL -->
]>

<!--
    HLP Test Cases
    
        * IMN_SHELL_CTL.BSM_CODE_CONFIG:            (scalar)
        * PARSER_SHELL_CTL.PA_PKT_META_FIFO_STATUS: [reg]
        * MSEC.MSEC_IP:                             [addrmap]
        * MSEC.MSEC_GLOBAL_LANE_CPORT_EN:           [addrmap][reg]
        * PARSER.PARSER_KEY_W:                      [regfile][reg]
        * SIA_RX.SIA_RX_Q_CFG:                      [addrmap]<regfile>[reg]
        * FFU_GROUP.FFU_ACTION:                     [addrmap][regfile][reg]

    MST Test Cases
        * WCM_ACTION                                [addrmap][regfile][reg]



    XML structure:

    <rdl>
        <addrmap name="(reg set name)">
            <addrmap name="(sub reg set name)">
               <regfile regfilename="(reg file name)"> (for multiply indexed registers only)
                   <reg regname="(reg name)"> (direct child of addrmap for non-multiply indexed)
                        <name>Brief description for set table</name>
                        <description>Longer description that goes right after individual
                                    register heading and before the register table.
                        </description>
                        <property name="(prop name)" value="(prop value or empty)"/>
                            .
                            .
                            .
                        <field fieldname="(field name)">
                            <name>(field name)</name>
                            <description>Field description</description>
                            <property name="(prop name)" value="(prop value or empty)"/>
                                .
                                .
                                .
                        </field>
                            .
                            .
                            .
                    </reg>
                        .
                        .
                        .
                </regfile>
            </addrmap>
        </addrmap>
            .
            .
            .
    </rdl>

-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

    <xsl:import href="hexMath.xsl" />

    <xsl:output method="html" version="1.0"
                encoding="iso-8859-1" indent="yes"
                doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"
                doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"/> 



    <!-- Command Line Parameters -->

    <!-- Set to 1 to make <description> elements from the XML
         appear in the HTML surrounded by <pre> tags. If the
         <description> content has not been formatted with
         line breaks, long entries will make the HTML page
         very wide.
         Set to 0 to consume <description> raw. If the
         <description> content has markdown (asterisks
         for bullets or ASCII drawings), they won't
         come out properly. -->
    <xsl:param name="descpre" select="0" />


    <!-- Global Variables -->

    <!-- Section heading for list of register sets -->
    <xsl:variable name="topHeading">
        <xsl:choose>
            <xsl:when test="/rdl/addrmap">
                <xsl:value-of select="'Top Level Register Maps'" />
            </xsl:when>

            <xsl:otherwise>
                <xsl:value-of select="'Register Summary'" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:variable>

    <!-- Number of hex digits to extend register addresses to -->
    <xsl:variable name="addrDigits" select="7" />

    <!-- headAndStyle -->

    <xsl:template name="headAndStyle">

		<head>
			<meta charset="utf-8"/>
			<title>Register Specification</title>
			<style>
				table { width: 100%; }
				table, th, td {
					text-align: left;
					border: 1px solid black;
					border-collapse: collapse;
				}
				th { background-color: #e6e6ff; }
				th, td { padding: 5px; }
				th.am1 { width: 25*; font-weight: bold; }
				th.am2 { width: 45*; font-weight: bold; }
				th.am3 { width: 15*; font-weight: bold; }
				th.am4 { width: 15*; font-weight: bold; }
				th.rf1 { width: 30*; font-weight: bold; }
				th.rf2 { width: 40*; font-weight: bold; }
				th.rf3 { width:  5*; font-weight: bold; }
				th.rf4 { width: 20*; font-weight: bold; }
				th.rg1 { width: 28*; font-weight: bold; }
				th.rg2 { width:  7*; font-weight: bold; }
				th.rg3 { width:  9*; font-weight: bold; }
				th.rg4 { width: 39*; font-weight: bold; }
				th.rg5 { width:  7*; font-weight: bold; }
				th.rg6 { width: 10*; font-weight: bold; }
			</style>
		</head>

    </xsl:template>


    <!-- Insert leading zeros into a hex value -->

    <xsl:template name="extendHexDigits">
        <!-- The hex value to extend, which should start with '0x' -->
        <xsl:param name="val" />
        <!-- The number of digits to extend to -->
        <xsl:param name="digits" select="$addrDigits" />

        <xsl:variable name="currentDigits">
            <xsl:value-of select="string-length(substring-after($val, '0x'))" />
        </xsl:variable>

        <xsl:choose>
            <xsl:when test="$digits &gt; $currentDigits">
                <xsl:variable name="extVal">
                    <xsl:text>0x0</xsl:text>
                    <xsl:value-of select="substring-after($val, '0x')" />
                </xsl:variable>

                <xsl:call-template name="extendHexDigits">
                    <xsl:with-param name="val" select="$extVal" />
                    <xsl:with-param name="digits" select="$digits" />
                </xsl:call-template>
            </xsl:when>

            <xsl:otherwise>
                <xsl:value-of select="$val" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>



    <!-- Provide register's base address, combining <reg> and <regfile>
         if the latter is present.
         Call only in <reg> context. -->

    <xsl:template name="regBase">
        <xsl:choose>
            <xsl:when test="parent::regfile">
                <xsl:call-template name="hexMath">
                    <xsl:with-param name="op1" select="../@addr" />
                    <xsl:with-param name="oper" select="'+'" />
                    <xsl:with-param name="op2" select="@addr" />
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="@addr" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>



    <!-- Get the step size of an array register -->

    <xsl:template name="arrayStepSize">
        <!-- The @addr attribute of the addrmap element, which should
             be of form: "[0]:0xXXXXXX [1]:0xYYYYYY ... [N]:0xZZZZZZ"  -->
        <xsl:param name="addr" />


        <xsl:variable name="first">
            <xsl:value-of select="substring-before(substring-after($addr, '[0]:0x'), ' [1]')" />
        </xsl:variable>

        <xsl:variable name="second">
            <xsl:value-of select="substring-before(substring-after($addr, '[1]:0x'), ' ')" />
        </xsl:variable>

        <xsl:call-template name="hexMath">
            <xsl:with-param name="op1" select="$second" />
            <xsl:with-param name="oper" select="'-'" />
            <xsl:with-param name="op2" select="$first" />
        </xsl:call-template>

    </xsl:template>


    <!-- Get the max index of an array register -->

    <xsl:template name="arrayMaxIndex">
        <!-- The @addr attribute of the addrmap element, which will
             be of form: "[0]:0xXXXXXX [1]:0xYYYYYY ... [N]:0xZZZZZZ"
             if it is an array.  -->
        <xsl:param name="addr" />
        <!-- The last index found in addr so far -->
        <xsl:param name="lastIndex" select="'0'" />

        <xsl:choose>
            <xsl:when test="starts-with($addr, '[')">
                <xsl:call-template name="arrayMaxIndex">
                    <xsl:with-param name="addr" select="substring-after($addr, ' ')" />
                    <xsl:with-param name="lastIndex" select="substring-after(substring-before($addr, ']'), '[')" />
                </xsl:call-template>
            </xsl:when>

            <xsl:otherwise>
                <xsl:value-of select="$lastIndex" />
            </xsl:otherwise>
        </xsl:choose>

    </xsl:template>


    <!-- Get the total size of an array register -->

    <xsl:template name="arraySize">
        <!-- The @addr attribute of the addrmap element, which should
             be of form: "[0]:0xXXXXXX [1]:0xYYYYYY ... [N]:0xZZZZZZ"  -->
        <xsl:param name="addr" />

        <xsl:variable name="stepSize">
            <xsl:call-template name="arrayStepSize">
                <xsl:with-param name="addr" select="$addr" />
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="decStepSize">
            <xsl:call-template name="hexToDec">
                <xsl:with-param name="hexVal" select="substring-after($stepSize, '0x')" />
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="maxIndex">
            <xsl:call-template name="arrayMaxIndex">
                <xsl:with-param name="addr" select="$addr" />
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="instances">
            <xsl:value-of select="$maxIndex + 1" />
        </xsl:variable>

        <xsl:variable name="decSize">
            <xsl:value-of select="$decStepSize * $instances"/>
        </xsl:variable>

        <xsl:variable name="exp">
            <xsl:call-template name="findpower">
                <xsl:with-param name="value" select="$decSize - 1"/>
                <xsl:with-param name="multiplier" select="2" />
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="size">
            <xsl:call-template name="printHex">
                <xsl:with-param name="number" select="$decStepSize * $instances"/>
            </xsl:call-template>
        </xsl:variable>

        <xsl:text>2^</xsl:text>
        <xsl:value-of select="$exp" />
        <xsl:text>=</xsl:text>
        <xsl:value-of select="$size"/>

    </xsl:template>


    <!-- Show register name as indexed array, if it is one.
         Only call from <reg> context! -->

    
    <xsl:template name="regArray">

        <!-- Get the @addr attribute for the parent (or grandparent)
             <addrmap> element -->
        <xsl:variable name="addr">
            <xsl:choose>
                <xsl:when test="parent::addrmap">
                    <xsl:value-of select="../@addr" />
                </xsl:when>
                <xsl:when test="parent::regfile">
                    <xsl:value-of select="../../@addr" />
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="'NO PARENT'" />
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <!-- Get the max array index for the (grand)parent <addrmap>.
             If it is not an array, the index will be zero. -->
        <xsl:variable name="index">
            <xsl:choose>
                <xsl:when test="$addr = 'NO PARENT'">
                    <xsl:value-of select="0" />
                </xsl:when>
                <xsl:otherwise>
                    <xsl:call-template name="arrayMaxIndex">
                        <xsl:with-param name="addr" select="$addr" />
                    </xsl:call-template>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        
        <xsl:value-of select="@regname" />
        <xsl:if test="$index &gt; 0">
            <xsl:text>[0..</xsl:text>
            <xsl:value-of select="$index" />
            <xsl:text>]</xsl:text>
        </xsl:if>

        <!-- If parent of <reg> is <regfile>, then this might be
             a multiply indexed register -->
        <xsl:if test="parent::regfile">
            <xsl:if test="../@num &gt; 1">
                <xsl:text>[0..</xsl:text>
                <xsl:value-of select="../@num - 1" />
                <xsl:text>]</xsl:text>
            </xsl:if>
        </xsl:if>

        <!-- Now check for the register itself to be indexed -->
        <xsl:if test="@num &gt; 1">
            <xsl:text>[0..</xsl:text>
            <xsl:value-of select="@num - 1" />
            <xsl:text>]</xsl:text>
        </xsl:if>
    </xsl:template>


    <!-- Create an href anchor -->

    <xsl:template name="href">
        <!-- The target anchor to hyperlink to -->
        <xsl:param name="target" />
        <!-- The display text for the hyperlink -->
        <xsl:param name="displayText" select="$target" />
        
        <A>
            <xsl:attribute name="href">
                <xsl:text>#</xsl:text>
                <xsl:value-of select="$target"/>
            </xsl:attribute>
            <xsl:value-of select="$displayText"/>
        </A>
    </xsl:template>


    <!-- Programmatic <Hx> Heading Level -->

    <xsl:template name="headingTag">

        <xsl:param name="level" select="3" />
        <xsl:param name="content" />

        <xsl:variable name="levelString" select="format-number($level, '#')" />

        <xsl:element name="h{$levelString}">
            <xsl:copy-of select="$content" />
        </xsl:element>

    </xsl:template>


    <!-- Make Anchor -->

    <xsl:template name="makeAnchor">
        <xsl:param name="string" />
        <xsl:param name="anchor" select="$string" />
        <!-- Create a hyperlink -->
        <xsl:param name="includeLink" select="0" />
        <xsl:param name="linkTarget" />

        <A>
            <xsl:attribute name="name">
                <xsl:value-of select="$anchor"/>
            </xsl:attribute>

            <xsl:if test="$includeLink = 1">
                <xsl:attribute name="href">
                    <xsl:text>#</xsl:text>
                    <xsl:value-of select="$linkTarget"/>
                </xsl:attribute>
            </xsl:if>

            <xsl:value-of select="$string"/>
        </A>
    </xsl:template>


    <!-- Create a subsection heading in the document -->

    <xsl:template name="makeHeading">
        <!-- Provide the text to use in the heading -->
        <xsl:param name="heading" select="name" />
        <!-- Specify the heading level, <Hx> -->
        <xsl:param name="level" />
        <!-- Create an anchor tag <A> by default -->
        <xsl:param name="includeAnchor" select="1" />
        <xsl:param name="anchor" />
        <!-- Create a hyperlink (includeAnchor must be 1) -->
        <xsl:param name="includeLink" select="0" />
        <xsl:param name="linkTarget" />

        <xsl:call-template name="headingTag">
            <xsl:with-param name="level" select="$level" />
            <xsl:with-param name="content">
                <xsl:choose>
                    <xsl:when test="$includeAnchor = 1">
                        <xsl:call-template name="makeAnchor">
                            <xsl:with-param name="string" select="$heading" />
                            <xsl:with-param name="anchor" select="$anchor" />
                            <xsl:with-param name="includeLink" select="$includeLink" />
                            <xsl:with-param name="linkTarget" select="$linkTarget" />
                        </xsl:call-template>
                    </xsl:when>

                    <xsl:otherwise>
                        <xsl:value-of select="$heading" />
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:with-param>
        </xsl:call-template>

    </xsl:template>


    <!-- Get <addrmap> base address -->
    <xsl:template name="regSetBaseAddr">
        <xsl:param name="context" select="." />

        <xsl:call-template name="extendHexDigits">
            <xsl:with-param name="val">
                <xsl:choose>
                    <xsl:when test="starts-with($context/@addr, '[0]:')">
                        <xsl:value-of select="substring-before(substring-after($context/@addr, '[0]:'), ' ')" />
                    </xsl:when>

                    <xsl:otherwise>
                        <xsl:value-of select="$context/@addr" />
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:with-param>
        </xsl:call-template>

    </xsl:template>


    <!-- Match <property>

         Note: Since there are a lot of properites that we don't
         care about, this tempalte is usually matched with a select
         that specifies the particular property by its name
         attriute:

            select="property[@name='(property name)']"
     -->

    <xsl:template match="property">
        <xsl:value-of select="@value" />
    </xsl:template>



    <!-- Match <field> -->

    <xsl:template match="field">
        <tr>
            <td>
                <xsl:value-of select="name" />
            </td>
            <td>
                <!-- Calc width from <msb> and <lsb> -->
                <xsl:value-of select="msb - lsb + 1" />
            </td>
            <td>
                <xsl:value-of select="msb" />
                <xsl:text>:</xsl:text>
                <xsl:value-of select="lsb" />
            </td>
            <td>
                <xsl:apply-templates select="description" mode="preformatted" />
            </td>
            <td>
                <xsl:apply-templates select="property[@name='AccessType']" />
            </td>
            <td>
                <xsl:apply-templates select="property[@name='reset']" />
            </td>
        </tr>
    </xsl:template>


    <!-- Match <description> -->

    <xsl:template match="description">
        <p>
            <xsl:apply-templates/>
        </p>
    </xsl:template>


    <!-- Handle bbcode list elements -->

    <xsl:template name="listElement">
        <xsl:param name="string" />

        <xsl:variable name="afterBullet" select="substring-after($string,'[*]')" />
        <xsl:variable name="thisBullet" select="substring-before($afterBullet,'[*]')" />
        <xsl:variable name="nextBullet" select="concat('[*]', substring-after($afterBullet,'[*]'))" />

        <xsl:element name="li">
            <xsl:choose>
                <!-- Not the last item in the list -->
                <xsl:when test="string-length($thisBullet) > 0">
                    <xsl:value-of select="$thisBullet" />
                </xsl:when>

                <!-- The last item in the list -->
                <xsl:when test="string-length($afterBullet) > 0">
                    <xsl:value-of select="$afterBullet" />
                </xsl:when>
            </xsl:choose>
        </xsl:element>

        <xsl:if test="string-length($nextBullet) > 3">
            <xsl:call-template name="listElement">
                <xsl:with-param name="string" select="$nextBullet" />
            </xsl:call-template>
        </xsl:if>

    </xsl:template>


    <!-- Decode bbcode markup -->

    <xsl:template name="bbcode">
        <xsl:param name="string" />

        <xsl:choose>
            <!-- Convert [code] into <pre> -->
            <xsl:when test="contains($string,'[code]') and contains($string,'[/code]')">
                <xsl:call-template name="bbcode">
                    <xsl:with-param name="string" select="substring-before($string, '[code]')" />
                </xsl:call-template>
                <xsl:element name="pre">
                    <xsl:value-of select="substring-before(substring-after($string, '[code]'),'[/code]')" />
                </xsl:element>
                <xsl:call-template name="bbcode">
                    <xsl:with-param name="string" select="substring-after($string, '[/code]')" />
                </xsl:call-template>
            </xsl:when>

            <!-- Convert [br] into <br/> -->
            <xsl:when test="contains($string,'[br]')">
                <xsl:value-of select="substring-before($string, '[br]')" />
                <xsl:element name="br" />
                <xsl:call-template name="bbcode">
                    <xsl:with-param name="string" select="substring-after($string, '[br]')" />
                </xsl:call-template>
            </xsl:when>

            <!-- Convert [list] into <ul> -->
            <xsl:when test="contains($string,'[list]') and contains($string,'[*]') and contains($string,'[/list]')">
                <xsl:call-template name="bbcode">
                    <xsl:with-param name="string" select="substring-before($string, '[list]')" />
                </xsl:call-template>

                <xsl:element name="ul">
                    <xsl:call-template name="listElement">
                        <xsl:with-param name="string" select="substring-before(substring-after($string, '[list]'),'[/list]')" />
                    </xsl:call-template>
                </xsl:element>

                <xsl:call-template name="bbcode">
                    <xsl:with-param name="string" select="substring-after($string, '[/list]')" />
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$string" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!-- Match <description> in preformatted mode -->

    <xsl:template match="description" mode="preformatted">
        <xsl:choose>
            <xsl:when test="$descpre">
                <xsl:element name="pre">
                    <xsl:value-of select="." />
                </xsl:element>
            </xsl:when>

            <xsl:when test="contains(., '[br]')  or contains(.,'[list]') or contains(.,'[code]')">
                <xsl:call-template name="bbcode">
                    <xsl:with-param name="string" select="text()" />
                </xsl:call-template>
            </xsl:when>

            <xsl:otherwise>
                <xsl:value-of select="." />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <!-- Match <copyright> -->

    <xsl:template match="copyright">
        <!-- Do nothing for now -->
    </xsl:template>


    <!-- Match <magillem_version> -->

    <xsl:template match="magillem_version">
        <!-- Do nothing for now -->
    </xsl:template>


    <!-- Match <creator> -->

    <xsl:template match="creator">
        <!-- Do nothing for now -->
    </xsl:template>


    <!-- Match <pubdate> -->

    <xsl:template match="pubdate">
        <p>
            <xsl:text>Published: </xsl:text>
            <xsl:apply-templates />
        </p>
    </xsl:template>


    <!-- Match <name> -->

    <xsl:template match="name">
        <!-- Do nothing for now -->
    </xsl:template>


    <!-- List the top level <addrmap> elements -->

    <xsl:template name="blocks">

        <!-- Summarize the blocks of register sets -->
        <table class="register-blocks" border="1" width="100%" cellpadding="5">
            <col width="25*"/>
            <col width="75*"/>
            <thead>
                <tr valign="top">
                    <th BGCOLOR="#e6e6ff">Block</th>
                    <th BGCOLOR="#e6e6ff">Description</th>
                </tr>
            </thead>
            <tbody>
                <xsl:apply-templates select="addrmap" mode="blockSummary" />
            </tbody>
        </table>

    </xsl:template>



    <!-- Match <addrmap> blockSummary mode -->

    <xsl:template match="addrmap" mode="blockSummary">

        <tr>
            <!-- Block Name -->
            <td>
                <xsl:call-template name="href">
                    <xsl:with-param name="target" select="@name" />
                </xsl:call-template>
            </td>

            <!-- Block Description -->
            <td>
                <xsl:value-of select="description" />
            </td>
        </tr>

    </xsl:template>


    <!-- Match <reg> details mode -->

    <xsl:template match="reg" mode="details" >
        <!-- Parent's name to use as anchor prefix -->
        <!-- Specify the heading level, <Hx> -->
        <xsl:param name="level" select="3" />
        <xsl:param name="parentTarget" />
        <xsl:param name="prefix" select="''" />

        <xsl:variable name="regName">
            <xsl:call-template name="regArray" />
        </xsl:variable>

        <xsl:call-template name="makeHeading">
            <xsl:with-param name="heading" select="$regName" />
            <xsl:with-param name="level" select="$level" />
            <xsl:with-param name="anchor" select="concat($prefix, $regName)" />
            <xsl:with-param name="includeLink" select="1" />
            <xsl:with-param name="linkTarget" select="$parentTarget" />
        </xsl:call-template>

        <p>
            <xsl:apply-templates select="description" mode="preformatted" />
        </p>

        <table class="register-details" border="1" width="100%" cellpadding="5">
            <col width="28*"/>
            <col width="7*"/>
            <col width="9*"/>
            <col width="39*"/>
            <col width="7*"/>
            <col width="10*"/>
            <thead>
                <tr>
                    <!-- addrmapX is the max index specified in the <addrmap> element 
                         and will be zero-based -->
                    <xsl:variable name="addrmapX">
                        <xsl:choose>
                            <xsl:when test="parent::addrmap">
                                <xsl:call-template name="arrayMaxIndex">
                                    <xsl:with-param name="addr" select="../@addr" />
                                </xsl:call-template>
                            </xsl:when>
                            <xsl:when test="parent::regfile">
                                <xsl:call-template name="arrayMaxIndex">
                                    <xsl:with-param name="addr" select="../../@addr" />
                                </xsl:call-template>
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:value-of select="0" />
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:variable>

                    <!-- regfileX is the max index specified in the <regfile> element.
                         If a <regfile> element exists, it will be one-based. -->
                    <xsl:variable name="regfileX">
                        <xsl:choose>
                            <xsl:when test="parent::regfile">
                                <xsl:value-of select="../@num" />
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:value-of select="1" />
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:variable>

                    <!-- We will assign 'i' for the first index, 'j' for the second
                         and 'k' for the third, whether they originate from 
                         <addrmap>, <regfile> or <reg> elements -->

                    <th>
                        <xsl:text>Address</xsl:text>
                        <xsl:choose>
                            <xsl:when test="$addrmapX &gt; 0">
                                <xsl:choose>
                                    <xsl:when test="$regfileX &gt; 1">
                                        <xsl:choose>
                                            <xsl:when test="@num &gt; 1">
                                                <!-- [addrmap][regfile][reg] -->
                                                <xsl:text>[i][j][k]</xsl:text>
                                            </xsl:when>
                                            <xsl:otherwise>
                                                <!-- [addrmap][regfile] -->
                                                <xsl:text>[i][j]</xsl:text>
                                            </xsl:otherwise>
                                        </xsl:choose>
                                    </xsl:when>
                                    
                                    <xsl:otherwise>
                                        <xsl:choose>
                                            <xsl:when test="@num &gt; 1">
                                                <!-- [addrmap][reg] -->
                                                <xsl:text>[i][j]</xsl:text>
                                            </xsl:when>
                                            <xsl:otherwise>
                                                <!-- [addrmap] -->
                                                <xsl:text>[i]</xsl:text>
                                            </xsl:otherwise>
                                        </xsl:choose>
                                    </xsl:otherwise>
                                </xsl:choose>
                            </xsl:when>
                            
                            <xsl:when test="$regfileX &gt; 1">
                                <xsl:choose>
                                    <xsl:when test="@num &gt; 1">
                                        <!-- [regfile][reg] -->
                                        <xsl:text>[i][j]</xsl:text>
                                    </xsl:when>
                                    <xsl:otherwise>
                                        <!-- [regfile] -->
                                        <xsl:text>[i]</xsl:text>
                                    </xsl:otherwise>
                                </xsl:choose>
                            </xsl:when>

                            <xsl:otherwise>
                                <xsl:if test="@num &gt; 1">
                                    <!-- [reg] -->
                                    <xsl:text>[i]</xsl:text>
                                </xsl:if>
                            </xsl:otherwise>
                        </xsl:choose>
                    </th>

                    <th colspan="5">
                        <!-- Start with base address of register set -->
                        <xsl:choose>
                            <xsl:when test="parent::addrmap">
                                <xsl:call-template name="regSetBaseAddr">
                                    <xsl:with-param name="context" select=".." />
                                </xsl:call-template>
                            </xsl:when>
                            <xsl:when test="parent::regfile">
                                <xsl:call-template name="regSetBaseAddr">
                                    <xsl:with-param name="context" select="../.." />
                                </xsl:call-template>
                            </xsl:when>
                        </xsl:choose>
                        
                        <!-- Incorporate regfile (if any) and register base address -->
                        <xsl:choose>
                            <xsl:when test="parent::addrmap or parent::regfile">
                                <xsl:text> + </xsl:text>
                                <xsl:call-template name="regBase" />
                            </xsl:when>
                            
                            <xsl:otherwise>
                                <xsl:call-template name="extendHexDigits">
                                    <xsl:with-param name="val">
                                        <xsl:call-template name="regBase" />
                                    </xsl:with-param>
                                </xsl:call-template>
                            </xsl:otherwise>
                        </xsl:choose>

                        <!-- Now deal with all the possible indexing scenarios -->
                        <xsl:choose>
                            <xsl:when test="$addrmapX &gt; 0">
                                <xsl:choose>
                                    <xsl:when test="$regfileX &gt; 1">
                                        <xsl:choose>
                                            <xsl:when test="@num &gt; 1">
                                                <!-- [addrmap][regfile][reg] -->
                                                <!-- addrmap part -->
                                                <xsl:variable name="stepSize">
                                                    <xsl:call-template name="arrayStepSize">
                                                        <xsl:with-param name="addr" select="../../@addr" />
                                                    </xsl:call-template>
                                                </xsl:variable>
                                                
                                                <xsl:text> + </xsl:text>
                                                <xsl:value-of select="$stepSize" />
                                                <xsl:text>*i for i:0..</xsl:text>
                                                <xsl:value-of select="$addrmapX" />

                                                <!-- regfile part -->
                                                <xsl:text> + </xsl:text>
                                                <xsl:value-of select="../@incr" />
                                                <xsl:text>*j for j:0..</xsl:text>
                                                <xsl:value-of select="$regfileX - 1" />

                                                <!-- reg part -->
                                                <xsl:text> + </xsl:text>
                                                <xsl:value-of select="@incr" />
                                                <xsl:text>*k for k:0..</xsl:text>
                                                <xsl:value-of select="@num - 1" />
                                            </xsl:when>

                                            <xsl:otherwise>
                                                <!-- [addrmap][regfile] -->
                                                <!-- addrmap part -->
                                                <xsl:variable name="stepSize">
                                                    <xsl:call-template name="arrayStepSize">
                                                        <xsl:with-param name="addr" select="../../@addr" />
                                                    </xsl:call-template>
                                                </xsl:variable>
                                                
                                                <xsl:text> + </xsl:text>
                                                <xsl:value-of select="$stepSize" />
                                                <xsl:text>*i for i:0..</xsl:text>
                                                <xsl:value-of select="$addrmapX" />

                                                <!-- regfile part -->
                                                <xsl:text> + </xsl:text>
                                                <xsl:value-of select="../@incr" />
                                                <xsl:text>*j for j:0..</xsl:text>
                                                <xsl:value-of select="$regfileX - 1" />
                                            </xsl:otherwise>
                                        </xsl:choose>
                                    </xsl:when>
                                    
                                    <xsl:otherwise>
                                        <xsl:choose>
                                            <xsl:when test="@num &gt; 1">
                                                <!-- [addrmap][reg] -->
                                                <!-- addrmap part -->
                                                <xsl:variable name="stepSize">
                                                    <xsl:choose>
                                                        <xsl:when test="parent::regfile">
                                                            <xsl:call-template name="arrayStepSize">
                                                                <xsl:with-param name="addr" select="../../@addr" />
                                                            </xsl:call-template>
                                                        </xsl:when>
                                                        <xsl:otherwise>
                                                            <xsl:call-template name="arrayStepSize">
                                                                <xsl:with-param name="addr" select="../@addr" />
                                                            </xsl:call-template>
                                                        </xsl:otherwise>
                                                    </xsl:choose>
                                                </xsl:variable>
                                                
                                                <xsl:text> + </xsl:text>
                                                <xsl:value-of select="$stepSize" />
                                                <xsl:text>*i for i:0..</xsl:text>
                                                <xsl:value-of select="$addrmapX" />

                                                <!-- reg part -->
                                                <xsl:text> + </xsl:text>
                                                <xsl:value-of select="@incr" />
                                                <xsl:text>*j for j:0..</xsl:text>
                                                <xsl:value-of select="@num - 1" />
                                            </xsl:when>

                                            <xsl:otherwise>
                                                <!-- [addrmap] -->
                                                <xsl:variable name="stepSize">
                                                    <xsl:choose>
                                                        <xsl:when test="parent::regfile">
                                                            <xsl:call-template name="arrayStepSize">
                                                                <xsl:with-param name="addr" select="../../@addr" />
                                                            </xsl:call-template>
                                                        </xsl:when>
                                                        <xsl:otherwise>
                                                            <xsl:call-template name="arrayStepSize">
                                                                <xsl:with-param name="addr" select="../@addr" />
                                                            </xsl:call-template>
                                                        </xsl:otherwise>
                                                    </xsl:choose>
                                                </xsl:variable>
                                                
                                                <xsl:text> + </xsl:text>
                                                <xsl:value-of select="$stepSize" />
                                                <xsl:text>*i for i:0..</xsl:text>
                                                <xsl:value-of select="$addrmapX" />
                                            </xsl:otherwise>
                                        </xsl:choose>
                                    </xsl:otherwise>
                                </xsl:choose>
                            </xsl:when>
                            
                            <xsl:when test="$regfileX &gt; 1">
                                <xsl:choose>
                                    <xsl:when test="@num &gt; 1">
                                        <!-- [regfile][reg] -->
                                        <!-- regfile part -->
                                        <xsl:text> + </xsl:text>
                                        <xsl:value-of select="../@incr" />
                                        <xsl:text>*i for i:0..</xsl:text>
                                        <xsl:value-of select="$regfileX - 1" />

                                        <!-- reg part -->
                                        <xsl:text> + </xsl:text>
                                        <xsl:value-of select="@incr" />
                                        <xsl:text>*j for j:0..</xsl:text>
                                        <xsl:value-of select="@num - 1" />
                                    </xsl:when>

                                    <xsl:otherwise>
                                        <!-- [regfile] -->
                                        <xsl:text> + </xsl:text>
                                        <xsl:value-of select="../@incr" />
                                        <xsl:text>*i for i:0..</xsl:text>
                                        <xsl:value-of select="$regfileX - 1" />
                                    </xsl:otherwise>
                                </xsl:choose>
                            </xsl:when>

                            <xsl:otherwise>
                                <xsl:if test="@num &gt; 1">
                                    <!-- [reg] -->
                                    <xsl:text> + </xsl:text>
                                    <xsl:value-of select="@incr" />
                                    <xsl:text>*i for i:0..</xsl:text>
                                    <xsl:value-of select="@num - 1" />
                                </xsl:if>
                            </xsl:otherwise>
                        </xsl:choose>

                    </th>
                </tr>

                <tr>
                    <th>Atomicity</th>
                    <th colspan="5">
                        <xsl:apply-templates select="property[@name='accesswidth']" />
                    </th>
                </tr>
                <tr>
                    <th>Reset Domains</th>
                    <th colspan="5">
                        <xsl:apply-templates select="property[@name='ResetDomains']" />
                    </th>
                </tr>
                <tr>
                    <th>Security Policy Group</th>
                    <th colspan="5">
                        <xsl:apply-templates select="property[@name='Security_PolicyGroup']" />
                    </th>
                </tr>
                <tr>
                    <th>Security Read Access</th>
                    <th colspan="5">
                        <xsl:apply-templates select="property[@name='Security_ReadAccess_Str']" />
                    </th>
                </tr>
                <tr>
                    <th>Security Write Access</th>
                    <th colspan="5">
                        <xsl:apply-templates select="property[@name='Security_WriteAccess_Str']" />
                    </th>
                </tr>
                <tr valign="top">
                    <th BGCOLOR="#e6e6ff">Field Name</th>
                    <th BGCOLOR="#e6e6ff">Width</th>
                    <th BGCOLOR="#e6e6ff">Bit</th>
                    <th BGCOLOR="#e6e6ff">Description</th>
                    <th BGCOLOR="#e6e6ff">Type</th>
                    <th BGCOLOR="#e6e6ff">Default</th>
                </tr>
            </thead>
            <tbody>
                <xsl:apply-templates select="field" />
            </tbody>
        </table>

    </xsl:template>


    <!-- Generate register summary table -->

    <xsl:template name="regSummaryTable">
        <xsl:param name="prefix" select="''" />

        <table class="register-summary" border="1" width="100%" cellpadding="5">
            <col width="30*"/>
            <col width="40*"/>
            <col width="10*"/>
            <col width="20*"/>
            <thead>
                <tr valign="top">
                    <th BGCOLOR="#e6e6ff">Register</th>
                    <th BGCOLOR="#e6e6ff">Description</th>
                    <th BGCOLOR="#e6e6ff">Atomicity</th>
                    <th BGCOLOR="#e6e6ff">Address</th>
                </tr>
            </thead>
            <tbody>
                <!-- Some <reg> will be direct children of the current
                     and some will be grandchildren through <regfile>, so we
                     must use .//reg to capture both children and
                     grandchildren. -->
                <xsl:apply-templates select=".//reg" mode="summary">
                    <xsl:with-param name="prefix" select="$prefix" />
                </xsl:apply-templates>
            </tbody>
        </table>
    </xsl:template>

   
    <!-- Match <reg> summary mode -->

    <xsl:template match="reg" mode="summary" >
        <xsl:param name="prefix" select="''" />

        <xsl:variable name="regName">
            <xsl:call-template name="regArray" />
        </xsl:variable>

        <tr>
            <td>
                <xsl:call-template name="href">
                    <xsl:with-param name="target" select="concat($prefix, $regName)" />
                    <xsl:with-param name="displayText" select="$regName" />
                </xsl:call-template>
            </td>
            <td>
                <xsl:value-of select="name" />
            </td>
            <td>
                <xsl:apply-templates select="property[@name='accesswidth']" />
            </td>
            <td>
                <!-- If there is a <regfile> element for this register, we
                     must include its base address -->
                <xsl:call-template name="extendHexDigits">
                    <xsl:with-param name="val">
                        <xsl:call-template name="regBase" />
                    </xsl:with-param>
                </xsl:call-template>
            </td>
        </tr>

    </xsl:template>


    <!-- Match <addrmap> - default case, which supports multi-level
         nesting of <addrmap> elements -->

    <xsl:template match="addrmap" mode="default">
        <!-- The <Hx> heading level -->
        <xsl:param name="level" />
        <!-- Where heading should link back to -->
        <xsl:param name="parentTarget" />
        <!-- "Path" of nested addrmaps through reg element:
             addrmap_addrmap_addrmap_reg -->
        <xsl:param name="prefix" select="''" />

        <xsl:variable name="anchor" select="concat($prefix, @name)" />
        <xsl:variable name="newPrefix" select="concat($anchor, '_')" />

        <!-- Heading this level <addrmap> -->
        <xsl:call-template name="makeHeading">
            <xsl:with-param name="heading" select="@name" />
            <xsl:with-param name="level" select="$level" />
            <xsl:with-param name="anchor" select="$anchor" />
            <xsl:with-param name="includeLink" select="'1'" />
            <xsl:with-param name="linkTarget" select="$parentTarget" />
        </xsl:call-template>

        <xsl:apply-templates select="description" />

        <xsl:choose>
            <xsl:when test="addrmap">
                <!-- Summarize the register sets (sub-addrmaps) that come 
                     under this addrmap. -->
                <xsl:call-template name="registerSets">
                    <xsl:with-param name="prefix" select="$newPrefix" />
                </xsl:call-template>

                <!-- Process sub-addrmaps -->
                <xsl:apply-templates select="addrmap" mode="default">
                    <xsl:with-param name="level" select="$level+1" />
                    <xsl:with-param name="parentTarget" select="$anchor" />
                    <xsl:with-param name="prefix" select="$newPrefix" />
                </xsl:apply-templates>
            </xsl:when>

            <xsl:otherwise>
                <!-- Now list the register details for each sub-addrmap -->
                <xsl:apply-templates select="." mode="registers">
                    <xsl:with-param name="level" select="$level+1" />
                    <xsl:with-param name="anchor" select="$anchor" />
                    <xsl:with-param name="prefix" select="$newPrefix" />
                </xsl:apply-templates>

            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!-- Match <addrmap> registers mode -->

    <xsl:template match="addrmap" mode="registers">
        <xsl:param name="level" select="2" />
        <xsl:param name="anchor" select="@name" />
        <xsl:param name="prefix" select="''" />

        <!-- Build a summary table of all registers in this set -->

        <xsl:call-template name="regSummaryTable">
            <xsl:with-param name="prefix" select="$prefix" />
        </xsl:call-template>

        <!-- Now give details for each register -->
        
        <xsl:apply-templates select=".//reg" mode="details">
            <xsl:with-param name="parentTarget" select="$anchor" />
            <xsl:with-param name="level" select="$level" />
            <xsl:with-param name="prefix" select="$prefix" />
        </xsl:apply-templates>

    </xsl:template>


    <!-- Match <addrmap> setSummary mode -->

    <xsl:template match="addrmap" mode="setSummary">
        <xsl:param name="prefix" select="''" />

        <xsl:variable name="addrBits">
            <xsl:apply-templates select="property[@name='AddressBits']" />
        </xsl:variable>

        <tr>
            <!-- Set Name -->
            <td>
                <xsl:call-template name="href">
                    <xsl:with-param name="target" select="concat($prefix, @name)" />
                    <xsl:with-param name="displayText" select="@name" />
                </xsl:call-template>
            </td>

            <!-- Set Description -->
            <td>
                <xsl:value-of select="description" />
            </td>

            <!-- Set Base Address -->
            <td>
                <xsl:call-template name="regSetBaseAddr" />
            </td>

            <!-- Set Base Size -->
            <td>
                <xsl:choose>
                    <xsl:when test="$addrBits = ''">
                        <xsl:call-template name="arraySize">
                            <xsl:with-param name="addr" select="@addr" />
                        </xsl:call-template>
                    </xsl:when>

                    <xsl:otherwise>
                        <xsl:text>2^</xsl:text>
                        <xsl:apply-templates select="property[@name='AddressBits']" />
                        <xsl:text>=</xsl:text>
                        <xsl:call-template name="printHex">
                            <xsl:with-param name="number">
                                <xsl:call-template name="raiseToPower">
                                    <xsl:with-param name="number" select="2" />
                                    <xsl:with-param name="power">
                                        <xsl:apply-templates select="property[@name='AddressBits']" />
                                    </xsl:with-param>
                                </xsl:call-template>
                            </xsl:with-param>
                        </xsl:call-template>
                    </xsl:otherwise>
                </xsl:choose>
            </td>
        </tr>

    </xsl:template>


    <!-- registerSets -->

    <xsl:template name="registerSets">
        <xsl:param name="prefix" select="''" />

        <!-- Summarize the register sets -->
        <table class="register-maps" border="1" width="100%" cellpadding="5">
            <col width="25*"/>
            <col width="45*"/>
            <col width="15*"/>
            <col width="15*"/>
            <thead>
                <tr valign="top">
                    <th BGCOLOR="#e6e6ff">Register Map</th>
                    <th BGCOLOR="#e6e6ff">Description</th>
                    <th BGCOLOR="#e6e6ff">Base Address</th>
                    <th BGCOLOR="#e6e6ff">Base Size</th>
                </tr>
            </thead>
            <tbody>
                <xsl:apply-templates select="addrmap" mode="setSummary">
                    <xsl:with-param name="prefix" select="$prefix" />
                </xsl:apply-templates>
            </tbody>
        </table>

    </xsl:template>


    <!-- Stat here: Match <rdl> -->

    <xsl:template match="/rdl">

        <xsl:variable name="baseLevel" select="1" />

        <html>
            <xsl:call-template name="headAndStyle" />
            <body>
                <h1>Register Set</h1>
                <xsl:apply-templates select="description" />
                <xsl:apply-templates select="pubdate" />

                <!-- Heading for top level summary table -->
                <xsl:call-template name="makeHeading">
                    <xsl:with-param name="heading" select="$topHeading" />
                    <xsl:with-param name="level" select="$baseLevel" />
                    <xsl:with-param name="anchor" select="$topHeading" />
                </xsl:call-template>
                
                <!-- If we have <addrmap> elements, then registers
                     are grouped into sets. If not, then just
                     dump the registers in a single flat list. -->

                <xsl:choose>
                    <!-- Original PE has no <addrmap> elements -->
                    <xsl:when test="not(addrmap)">
                        <xsl:call-template name="regSummaryTable" />

                        <xsl:apply-templates select="reg" mode="details">
                            <xsl:with-param name="parentTarget" select="''" />
                            <xsl:with-param name="level" select="2" />
                        </xsl:apply-templates>
                    </xsl:when>

                    <xsl:otherwise>
                        <xsl:call-template name="blocks" />
                        <xsl:apply-templates select="addrmap" mode="default">
                            <xsl:with-param name="level" select="$baseLevel + 1" />
                            <xsl:with-param name="parentTarget" select="$topHeading" />
                        </xsl:apply-templates>
                    </xsl:otherwise>
                </xsl:choose>
            </body>
        </html>
    </xsl:template>

</xsl:stylesheet>
