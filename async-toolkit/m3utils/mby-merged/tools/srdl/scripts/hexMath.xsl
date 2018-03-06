<?xml version="1.0"?>
<xsl:transform version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">


    <!-- Do an arithmetic operation (+, -, *, div, mod) on two  hex values -->

    <xsl:template name="hexMath">
        <xsl:param name="op1"/>
        <xsl:param name="oper"/>
        <xsl:param name="op2"/>

        <xsl:variable name="numOp1">
            <xsl:call-template name="hexToDec">
                <xsl:with-param name="hexVal" select="$op1"/>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="numOp2">
            <xsl:call-template name="hexToDec">
                <xsl:with-param name="hexVal" select="$op2"/>
            </xsl:call-template>
        </xsl:variable>

        <xsl:choose>

            <xsl:when test="$oper = '+'">
                <xsl:variable name="res">
                    <xsl:call-template name="printHex">
                        <xsl:with-param name="number" select="$numOp1 + $numOp2"/>
                    </xsl:call-template>
                </xsl:variable>
                <xsl:value-of select="$res"/>
            </xsl:when>

            <xsl:when test="$oper = '-'">
                <xsl:variable name="res">
                    <xsl:call-template name="printHex">
                        <xsl:with-param name="number" select="$numOp1 - $numOp2"/>
                    </xsl:call-template>
                </xsl:variable>
                <xsl:value-of select="$res"/>
            </xsl:when>

            <xsl:when test="$oper = '*'">
                <xsl:variable name="res">
                    <xsl:call-template name="printHex">
                        <xsl:with-param name="number" select="$numOp1 * $numOp2"/>
                    </xsl:call-template>
                </xsl:variable>
                <xsl:value-of select="$res"/>
            </xsl:when>

            <xsl:when test="$oper = 'div'">
                <xsl:variable name="res">
                    <xsl:call-template name="printHex">
                        <xsl:with-param name="number" select="$numOp1 div $numOp2"/>
                    </xsl:call-template>
                </xsl:variable>
                <xsl:value-of select="$res"/>
            </xsl:when>
            
            <xsl:when test="$oper = 'mod'">
                <xsl:variable name="res">
                    <xsl:call-template name="printHex">
                        <xsl:with-param name="number" select="$numOp1 mod $numOp2"/>
                    </xsl:call-template>
                </xsl:variable>
                <xsl:value-of select="$res"/>
            </xsl:when>

            <xsl:otherwise>
                <xsl:message>Error!  Unknown operation!</xsl:message>
            </xsl:otherwise>

        </xsl:choose>
        
    </xsl:template>
  

    <!-- Given a decimal value between 1 and 15, return its hex equivalent -->

    <xsl:template name="singleDecToHex">
        <xsl:param name="num"/>

        <xsl:choose>
            <xsl:when test="$num &lt; 16 and $num &gt;= 0">
                <xsl:variable name="table" select="'0123456789ABCDEF'"/>
                <xsl:value-of select="substring($table,$num + 1,1)"/>
            </xsl:when>

            <xsl:otherwise>
                <xsl:message>
                Number to convert to hexadecimal out of range</xsl:message>
            </xsl:otherwise>

        </xsl:choose>

    </xsl:template>
  

    <!-- Given a single hex digit, return its decimal equivalent -->

    <xsl:template name="singleHexToDec">
        <xsl:param name="hex"/>

        <xsl:variable name="table" select="'0123456789abcdef'"/>
        <xsl:value-of select="string-length(substring-before($table,translate($hex, 'ABCDEF',
        'abcdef')))"/>
    </xsl:template>

    <xsl:template name="findpower">
        <xsl:param name="value"/>
        <xsl:param name="currpower" select="0"/>
        <xsl:param name="multiplier"/>
        <xsl:param name="accumulator" select="1"/>
        
        <xsl:choose>
            <xsl:when test="$value &lt; $accumulator">
                <xsl:value-of select="$currpower"/>
            </xsl:when>

            <xsl:otherwise>
                <xsl:call-template name="findpower">
                    <xsl:with-param name="value" select="$value"/>
                    <xsl:with-param name="currpower" select="$currpower + 1"/>
                    <xsl:with-param name="multiplier" select="$multiplier"/>
                    <xsl:with-param name="accumulator" select="$accumulator * $multiplier"/>
                </xsl:call-template>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="raiseToPower">
        <xsl:param name="number"/>
        <xsl:param name="power"/>

        <xsl:call-template name="raiseToPowerIter">
            <xsl:with-param name="multiplier" select="$number"/>
            <xsl:with-param name="accumulator" select="1"/>
            <xsl:with-param name="reps" select="$power"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template name="raiseToPowerIter">
        <xsl:param name="multiplier"/>
        <xsl:param name="accumulator"/>
        <xsl:param name="reps"/>

        <xsl:choose>
            <xsl:when test="$reps &gt; 0">
                <xsl:call-template name="raiseToPowerIter">
                    <xsl:with-param name="multiplier" select="$multiplier"/>
                    <xsl:with-param name="accumulator" select="$accumulator * $multiplier"/>
                    <xsl:with-param name="reps" select="$reps - 1"/>
                </xsl:call-template>
            </xsl:when>

            <xsl:otherwise>
                <xsl:value-of select="$accumulator"/>
            </xsl:otherwise>
        </xsl:choose>

    </xsl:template>

    <xsl:template name="hexToDec">
        <xsl:param name="hexVal"/>
        <xsl:param name="decVal" select="0"/>

        <xsl:variable name="hexLength" select="string-length($hexVal)"/>

        <xsl:choose>
            <xsl:when test="$hexLength &gt; 0">
                <xsl:variable name="hexPos">
                    <xsl:call-template name="singleHexToDec">
                        <xsl:with-param name="hex" select="substring($hexVal,1,1)"/>
                    </xsl:call-template>
                </xsl:variable>

                <xsl:variable name="addToDec">
                    <xsl:call-template name="raiseToPower">
                        <xsl:with-param name="number" select="16"/>
                        <xsl:with-param name="power" select="$hexLength - 1"/>
                    </xsl:call-template>
                </xsl:variable>

                <xsl:call-template name="hexToDec">
                    <xsl:with-param name="hexVal" select="substring($hexVal,2)"/>
                    <xsl:with-param name="decVal" select="$decVal + ($addToDec * $hexPos)"/>
                </xsl:call-template>
            </xsl:when>

            <xsl:otherwise>
                <xsl:value-of select="$decVal"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>



    <xsl:template name="printHex">
        <xsl:param name="number">0</xsl:param>

        <xsl:variable name="low">
            <xsl:value-of select="$number mod 16"/>
        </xsl:variable>

        <xsl:variable name="high">
            <xsl:value-of select="floor($number div 16)"/>
        </xsl:variable>

        <xsl:choose>
            <xsl:when test="$high &gt; 0">
                <xsl:call-template name="printHex">
                    <xsl:with-param name="number">
                        <xsl:value-of select="$high"/>
                    </xsl:with-param>
                </xsl:call-template>  
            </xsl:when>

            <xsl:otherwise>
                <xsl:text>0x</xsl:text>
            </xsl:otherwise>
        </xsl:choose>  

        <xsl:choose>
            <xsl:when test="$low &lt; 10">
                <xsl:value-of select="$low"/>
            </xsl:when>

            <xsl:otherwise>
                <xsl:variable name="temp">
                    <xsl:value-of select="$low - 10"/>
                </xsl:variable>

                <xsl:value-of select="translate($temp, '012345', 'ABCDEF')"/>
            </xsl:otherwise>
        </xsl:choose>

    </xsl:template>  

</xsl:transform>
