using System;
using System.Collections.Generic;
using System.Text;
using SPAGS.Util;

namespace SPAGS
{
    public partial class ScriptParser
    {
        private int preprocLevel = 0;
        private int preprocSuccess = 0;
        private bool PreprocBlockingTokenStream
        {
            get { return (preprocLevel > preprocSuccess); }
        }

        private bool skipWhitespaceAndComments()
        {
            while (pos < endPos)
            {
                if (Char.IsWhiteSpace(source, pos))
                {
                    if (source[pos] == '\n')
                    {
                        return true;
                    }
                    pos++;
                    continue;
                }
                if (source[pos] == '/' && (pos + 1) < endPos)
                {
                    if (source[pos + 1] == '/')
                    {
                        while (pos < endPos && source[pos] != '\n') pos++;
                        return true;
                    }
                    else if (source[pos + 1] == '*')
                    {
                        pos += 3;
                        if (pos >= endPos) throw new Exception("unterminated comment");
                        bool endOfLine = (source[pos - 1] == '\n');
                        for (; ; )
                        {
                            if (source[pos] == '/' && source[pos - 1] == '*') break;
                            if (source[pos] == '\n') endOfLine = true;
                            pos++;
                            if (pos >= endPos) throw new Exception("unterminated comment");
                        }
                        pos++;
                        return endOfLine;
                    }
                }
                // hit something that is not a new line or a comment
                return false;
            }
            return true;
        }

        private void CheckPreprocessorBalanced()
        {
            if (preprocLevel > 0) throw new Exception("#if without #endif");
        }

        private Version AdvanceVersion()
        {
            int start = pos;
            while (pos < endPos && source[pos] != '\n') pos++;
            string versionString = source.Substring(start, pos - start).Trim();
            if (versionString.IndexOf('.') == -1) versionString += ".0";
            return new Version(versionString);
        }

        private void HandlePreprocessorInstruction()
        {
            preprocLineMode = true;
            preprocWordMode = true;
            try
            {
                AdvanceToken();

                string instruction = CurrentName;
                if (instruction == null) throw new Exception("invalid preprocessor instruction");

                switch (instruction)
                {
                    case "define":
                        AdvanceToken();
                        int restorePos = pos;
                        string defineName = AdvanceName();
                        Expression constantExpression;
                        preprocWordMode = false;
                        try
                        {
                            constantExpression = AdvanceExpression();
                            if (token.Type != TokenType.EndOfInput || !constantExpression.IsConstant())
                            {
                                constantExpression = null;
                            }
                        }
                        catch { constantExpression = null; }
                        if (constantExpression != null)
                        {
                            Constant.Expression constant = new Constant.Expression(defineName, constantExpression);
                            constant.OwnerScript = script;
                            script.DefinedConstants.Add(constant);
                            if (preprocLevel == preprocSuccess)
                            {
                                /*
                                 * The "Stack" module shows that #define CAN be used to override things that already exist:
                                 * 
                                 * managed struct StackData {};
                                 * #define StackData String
                                 * 
                                 * (it does that for autocomplete purposes)
                                 */
                                if (Namespace.ContainsKey(constant.Name)) Namespace.Remove(constant.Name);
                                Namespace.Add(constant);
                            }
                            return;
                        }
                        pos = restorePos;
                        preprocWordMode = true;
                        AdvanceToken();
                        List<Token> readTokens = new List<Token>();
                        while (token.Type != TokenType.EndOfInput)
                        {
                            readTokens.Add(token);
                            AdvanceToken();
                        }
                        Constant.TokenSequence fragment = new Constant.TokenSequence(defineName, readTokens);
                        script.DefinedConstants.Add(fragment);
                        fragment.OwnerScript = script;
                        if (preprocSuccess == preprocLevel)
                        {
                            /*
                             * The "Stack" module shows that #define CAN be used to override things that already exist:
                             * 
                             * managed struct StackData {};
                             * #define StackData String
                             * 
                             * (it does that for autocomplete purposes)
                             */
                            if (Namespace.ContainsKey(fragment.Name)) Namespace.Remove(fragment.Name);
                            Namespace.Add(fragment);
                        }
                        return;
                    case "ifver":
                        if (engineVersion >= AdvanceVersion() && preprocLevel == preprocSuccess)
                        {
                            preprocSuccess++;
                        }
                        preprocLevel++;
                        return;
                    case "ifnver":
                        if (engineVersion < AdvanceVersion() && preprocLevel == preprocSuccess)
                        {
                            preprocSuccess++;
                        }
                        preprocLevel++;
                        return;
                    case "ifdef":
                        AdvanceToken();
                        string ifdefName = AdvanceName();
                        if (token.Type != TokenType.EndOfInput) throw new Exception("invalid #ifdef");
                        if (preprocLevel == preprocSuccess && Namespace.ContainsKey(ifdefName))
                        {
                            preprocSuccess++;
                        }
                        preprocLevel++;
                        return;
                    case "undef":
                        AdvanceToken();
                        INameHolder defined;
                        string undefMe = AdvanceName();
                        if (!Namespace.TryGetValue(undefMe, out defined) || !(defined is Constant))
                        {
                            throw new Exception("attempt to #undef an undefined value: " + undefMe);
                        }
                        if (token.Type != TokenType.EndOfInput) throw new Exception("invalid #undef");

                        Constant toUndefine = (Constant)defined;
                        toUndefine.Undefined = true;
                        Namespace.Remove(toUndefine);
                        return;
                    case "error":
                        int startMessage = pos;
                        while (pos < endPos && source[pos] != '\r' && source[pos] != '\n') pos++;
                        if (preprocLevel == preprocSuccess)
                        {
                            throw new Exception(source.Substring(startMessage, pos - startMessage));
                        }
                        return;
                    case "ifndef":
                        AdvanceToken();
                        string ifndefName = AdvanceName();
                        if (token.Type != TokenType.EndOfInput) throw new Exception("invalid #ifndef");
                        if (preprocLevel == preprocSuccess && !Namespace.ContainsKey(ifndefName))
                        {
                            preprocSuccess++;
                        }
                        preprocLevel++;
                        return;
                    case "endif":
                        if (preprocLevel < 1) throw new Exception("#endif without #if");
                        if (--preprocLevel < preprocSuccess) preprocSuccess = preprocLevel;
                        AdvanceToken();
                        if (token.Type != TokenType.EndOfInput)
                        {
                            throw new Exception("unexpected content after #endif");
                        }
                        return;
                    case "sectionstart":
                    case "sectionend":
                        // ignore
                        while (pos < endPos && source[pos] != '\n') pos++;
                        return;
                    default:
                        throw new Exception("unsupported instruction: #" + token);
                }
            }
            finally
            {
                preprocLineMode = false;
                preprocWordMode = false;
            }
        }
    }
}
