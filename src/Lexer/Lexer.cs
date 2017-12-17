using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;

/*
The MIT License (MIT)

Copyright (c) 2013 Oleg Shevchenko

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
*/

namespace System.Text
{
    public class Lexer : IEnumerable<Token>, IEnumerator<Token>
    {
        private readonly LexerSettings _settings;
        private LexerBehavior _behavior;
        private TextReader _reader;
        private string _text;
        private int _position;
        private int _start;
        private int _textLen;
        private int _textPos;
        private int _textBeg;
        private int _bufBeg;
        private int _maxSymLen;
        private int _lineBegin;
        private int _lineNumber;
        private int _endLineBegin;
        private int _endLineNumber;
        private StringBuilder _buffer;
        private StringBuilder _tokenBuffer;
        private Token _current;
        private Token _next;

        private Lexer(string text, TextReader reader, LexerBehavior behavior, LexerSettings settings)
        {
            if (settings == null)
            {
                settings = LexerSettings.Default;
            }
            else
            {
                settings = settings.Clone();
            }

            _text = text;
            _reader = reader;
            _behavior = behavior;
            _settings = settings;

            if (settings.Symbols != null)
            {
                foreach (KeyValuePair<string, int> entry in settings.Symbols)
                {
                    int len = entry.Key.Length;
                    if (len > _maxSymLen)
                    {
                        _maxSymLen = len;
                    }
                }
            }

            Reset();
        }

        public Lexer(string text, LexerBehavior behavior, LexerSettings settings)
            : this(text, null, behavior, settings)
        {
        }

        public Lexer(string text, LexerBehavior behavior)
            : this(text, null, behavior, null)
        {
        }

        public Lexer(string text, LexerSettings settings)
            : this(text, null, LexerBehavior.Default, settings)
        {
        }

        public Lexer(string text)
            : this(text, null, LexerBehavior.Default, null)
        {
        }

        public Lexer(TextReader reader, LexerBehavior behavior, LexerSettings settings)
            : this(null, reader, behavior, settings)
        {
        }

        public Lexer(TextReader reader, LexerBehavior behavior)
            : this(null, reader, behavior, null)
        {
        }

        public Lexer(TextReader reader, LexerSettings settings)
            : this(null, reader, LexerBehavior.Default, settings)
        {
        }

        public Lexer(TextReader reader)
            : this(null, reader, LexerBehavior.Default, null)
        {
        }

        private const int BufferCapacity = 8192;

        private const char EndOfTextChar = unchecked((char)-1);

        public Token Current
        {
            get
            {
                return _current;
            }
        }

        public bool IsEmpty
        {
            get
            {
                return _text == null;
            }
        }

        public void Reset()
        {
            int readerPos = _position - _textPos;
            _current = new Token(TokenType.Start, null, null, CommonLexem.Start, 0, 0, 0, 0, 0, 0);
            _next = null;
            _textPos = 0;
            _position = 0;
            _textBeg = 0;
            _tokenBuffer = null;
            _buffer = null;
            _bufBeg = -1;

            if (_reader != null)
            {
                if (_text != null && readerPos > 0)
                {
                    StreamReader streamReader = _reader as StreamReader;
                    if (streamReader != null && streamReader.BaseStream.CanSeek)
                    {
                        streamReader.BaseStream.Seek(0, SeekOrigin.Begin);
                        _text = null;
                    }
                }

                if (_text == null)
                {
                    _textLen = 0;
                    ReadCharBuffer();
                }
            }
            else
            {
                _textLen = (_text == null ? 0 : _text.Length);
            }
        }

        public Token GetNextToken(LexerBehavior behavior)
        {
            LexerBehavior saveBehavior = _behavior;
            _behavior = behavior;
            try
            {
                return GetNextToken();
            }
            finally
            {
                _behavior = saveBehavior;
            }
        }

        public Token GetNextToken()
        {
            if (_next != null)
            {
                _current = _next;
                _next = null;
            }
            else
            {
                _current = GetToken();
            }

            return _current;
        }

        public Token PeekNextToken(LexerBehavior behavior)
        {
            LexerBehavior saveBehavior = _behavior;
            _behavior = behavior;
            try
            {
                return PeekNextToken();
            }
            finally
            {
                _behavior = saveBehavior;
            }
        }

        public Token PeekNextToken()
        {
            if (_next == null)
            {
                _next = GetToken();
            }

            return _next;
        }

        #region Private Implementation

        private Token GetToken()
        {
            if (_text == null)
            {
                return new Token(TokenType.End, "", "", CommonLexem.End, 0, 0, 0, 0, 0, 0);
            }

            _lineBegin = _endLineBegin;
            _lineNumber = _endLineNumber;
            _start = _position;
            _textBeg = _textPos;
            _bufBeg = -1;
            _tokenBuffer = null;
            _buffer = null;

            char currentChar = PeekChar();
            bool skip;
            do
            {
                skip = false;
                // end
                if (currentChar == EndOfTextChar && EndOfText())
                {
                    return GetEndToken();
                }

                // separator
                if (currentChar <= ' ')
                {
                    bool skipWhiteSpaces = (_behavior & LexerBehavior.SkipWhiteSpaces) != 0;
                    do
                    {
                        ReadNext();
                        if (skipWhiteSpaces)
                        {
                            _textBeg = _textPos;
                        }

                        if (EndOfLine(currentChar))
                        {
                            if (skipWhiteSpaces)
                            {
                                _textBeg = _textPos;
                            }
                            else if ((_settings.Options & LexerOptions.EndOfLineAsToken) != 0)
                            {
                                return new Token(TokenType.EndOfLine, "", GetTokenText(), 0, _start, _position, _lineBegin, _lineNumber, _endLineBegin, _endLineNumber);
                            }
                        }

                        currentChar = PeekChar();
                        if (currentChar == EndOfTextChar && EndOfText())
                        {
                            break;
                        }

                    } while (currentChar <= ' ');

                    if (!skipWhiteSpaces)
                    {
                        return new Token(TokenType.WhiteSpace, "", GetTokenText(), 0, _start, _position, _lineBegin, _lineNumber, _endLineBegin, _endLineNumber);
                    }

                    _textBeg = _textPos;
                    skip = true;
                    _start = _position;
                }

                // inline comment
                string[] inlineComments = _settings.InlineComments;
                if (inlineComments != null)
                {
                    for (int inlineCommentIndex = 0; inlineCommentIndex < inlineComments.Length; inlineCommentIndex++)
                    {
                        string inlineComment = inlineComments[inlineCommentIndex];
                        if (NextSymbolIs(inlineComment))
                        {
                            bool skipComments = ((_behavior & LexerBehavior.SkipComments) != 0);
                            skip = true;
                            if (skipComments)
                            {
                                _textBeg = _textPos;
                            }

                            currentChar = PeekChar();
                            while (true)
                            {
                                if (currentChar == '\r' || currentChar == '\n')
                                {
                                    break;
                                }

                                currentChar = NextChar();
                                if (currentChar == EndOfTextChar && EndOfText())
                                {
                                    break;
                                }

                                if (skipComments)
                                {
                                    _textBeg = _textPos;
                                }
                            }

                            if (skipComments)
                            {
                                _start = _position;
                            }
                            else
                            {
                                return new Token(TokenType.Comment, "", GetTokenText(), 0, _start, _position, _lineBegin, _lineNumber, _lineBegin, _lineNumber);
                            }

                            break;
                        }
                    }
                }

                // comment
                if (!string.IsNullOrEmpty(_settings.CommentBegin) && NextSymbolIs(_settings.CommentBegin))
                {
                    bool skipComments = ((_behavior & LexerBehavior.SkipComments) != 0);
                    skip = true;
                    if (skipComments)
                    {
                        _textBeg = _textPos;
                    }

                    while (true)
                    {
                        if (NextSymbolIs(_settings.CommentEnd))
                        {
                            currentChar = PeekChar();
                            if (skipComments)
                            {
                                _textBeg = _textPos;
                            }

                            break;
                        }

                        currentChar = NextChar();
                        if (currentChar == EndOfTextChar && EndOfText())
                        {
                            break;
                        }
                        else
                        {
                            EndOfLine(currentChar);
                        }

                        if (skipComments)
                        {
                            _textBeg = _textPos;
                        }
                    }

                    if (skipComments)
                    {
                        _start = _position;
                    }
                    else
                    {
                        return new Token(TokenType.Comment, "", GetTokenText(), 0, _start, _position, _lineBegin, _lineNumber, _endLineBegin, _endLineNumber);
                    }
                }

                _lineNumber = _endLineNumber;
                _lineBegin = _endLineBegin;

            } while (skip);

            // quoted string
            char[] stringQuotes = _settings.StringQuotes;
            if (stringQuotes != null)
            {
                for (int i = 0; i < stringQuotes.Length; i++)
                {
                    char stringQuoteChar = stringQuotes[i];
                    if (currentChar == stringQuoteChar || i == 0 && currentChar == _settings.StringPrefix && PeekChar(1) == stringQuoteChar)
                    {
                        return GetQuotedStringToken(currentChar != stringQuoteChar, stringQuoteChar);
                    }
                }
            }

            // quoted identifier
            bool isIdentQuote = currentChar == _settings.IdentQuote;
            bool quote = isIdentQuote || currentChar == _settings.IdentQuoteBegin;
            char nextChar;
            if (quote || currentChar == _settings.IdentPrefix && (isIdentQuote = (nextChar = PeekChar(1)) == _settings.IdentQuote || nextChar == _settings.IdentQuoteBegin))
            {
                return GetQuotedIdentifierToken(!quote, isIdentQuote);
            }

            // prefix identifier
            if (currentChar == _settings.IdentPrefix)
            {
                return GetPrefixedIdentifierToken();
            }

            // number
            if (currentChar >= '0' && currentChar <= '9')
            {
                return GetNumberToken(currentChar);
            }

            // keyword / identifier
            if (Char.IsLetter(currentChar) || currentChar == '_' || IsIdentChar(currentChar))
            {
                return GetKeywordOrIdentifierToken(currentChar);
            }

            // predefined symbol
            if (_settings.Symbols != null)
            {
                string symbol = PeekSubstring(_maxSymLen);
                for (int i = symbol.Length; i > 0; i--, symbol = symbol.Substring(0, i))
                {
                    int symbolId;
                    if (_settings.Symbols.TryGetValue(symbol, out symbolId))
                    {
                        Skip(i);
                        string symbolText = (_behavior & LexerBehavior.PersistTokenText) != 0 ? symbol : null;
                        return new Token(TokenType.Symbol, symbol, symbolText, (int)symbolId, _start, _position, _lineBegin, _lineNumber, _lineBegin, _lineNumber);
                    }
                }
            }

            // just a char
            currentChar = NextChar();
            string charText = (_behavior & LexerBehavior.PersistTokenText) != 0 ? currentChar.ToString() : null;
            return new Token(TokenType.Char, currentChar, charText, 0, _start, _position, _lineBegin, _lineNumber, _lineBegin, _lineNumber);
        }

        private Token GetEndToken()
        {
            if (_reader != null)
            {
                _reader.Close();
            }

            return new Token(TokenType.End, "", "", CommonLexem.End, _start, _start, _lineBegin, _lineNumber, _lineBegin, _lineNumber);
        }

        private Token GetQuotedIdentifierToken(bool prefix, bool isIdentQuote)
        {
            if (prefix)
            {
                ReadNext();
            }

            char quoteEnd;
            bool doubleQuote;
            if (isIdentQuote)
            {
                quoteEnd = _settings.IdentQuote;
                doubleQuote = (_settings.Options & LexerOptions.IdentDoubleQuote) != 0;
            }
            else
            {
                quoteEnd = _settings.IdentQuoteEnd;
                doubleQuote = false;
            }

            ReadNext();
            _bufBeg = _textPos;

            while (true)
            {
                char currentChar = NextChar();
                BufferAdd(currentChar);

                if (currentChar == quoteEnd)
                {
                    if (doubleQuote && PeekChar() == quoteEnd)
                    {
                        EnsureBuffer(1);
                        currentChar = NextChar();
                        BufferAdd(currentChar);
                    }
                    else
                    {
                        break;
                    }
                }

                if (currentChar == EndOfTextChar && EndOfText())
                {
                    break;
                }
                else
                {
                    EndOfLine(currentChar);
                }
            }

            string val = GetBufferValue(-1);
            return new Token(TokenType.Identifier, val, GetTokenText(), 0, _start, _position, _lineBegin, _lineNumber, _endLineBegin, _endLineNumber);
        }

        private Token GetQuotedStringToken(bool prefix, char stringQuoteChar)
        {
            char escapeChar;
            bool escaping;
            bool doubleQuote;
            if (prefix)
            {
                escapeChar = '\0';
                escaping = false;
                doubleQuote = true;
                ReadNext();
            }
            else
            {
                escapeChar = _settings.StringEscapeChar;
                escaping = (_settings.Options & LexerOptions.StringEscaping) != 0;
                doubleQuote = (_settings.Options & LexerOptions.StringDoubleQuote) != 0;
            }

            ReadNext();
            _bufBeg = _textPos;

            while (true)
            {
                char currentChar = NextChar();
                BufferAdd(currentChar);

                if (currentChar == escapeChar && escaping)
                {
                    EnsureBuffer(1);
                    currentChar = NextChar();
                    BufferAdd(currentChar);
                }
                else if (currentChar == stringQuoteChar)
                {
                    if (doubleQuote && PeekChar() == stringQuoteChar)
                    {
                        EnsureBuffer(1);
                        currentChar = NextChar();
                        BufferAdd(currentChar);
                    }
                    else
                    {
                        break;
                    }
                }
                else if (currentChar == EndOfTextChar && EndOfText())
                {
                    break;
                }
                else
                {
                    EndOfLine(currentChar);
                }
            }

            string val = GetBufferValue(-1);
            return new Token(TokenType.QuotedString, val, GetTokenText(), 0, _start, _position, _lineBegin, _lineNumber, _endLineBegin, _endLineNumber);
        }

        private Token GetKeywordOrIdentifierToken(char currentChar)
        {
            _bufBeg = _textPos;
            do
            {
                ReadNext();
                BufferAdd(currentChar);
                currentChar = PeekChar();
            } while (Char.IsLetterOrDigit(currentChar) || currentChar == '_' || IsIdentChar(currentChar));

            string val = GetBufferValue(0);

            int id = 0;
            TokenType tokenType = TokenType.Identifier;
            if ((_settings.Options & LexerOptions.IdentToUpper) != 0)
            {
                val = val.ToUpper(_settings.CultureInfo);
                if (_settings.Keywords != null && _settings.Keywords.TryGetValue(val, out id))
                {
                    tokenType = TokenType.Keyword;
                }
            }
            else
            {
                if (_settings.Keywords != null && _settings.Keywords.TryGetValue(val.ToUpper(_settings.CultureInfo), out id))
                {
                    tokenType = TokenType.Keyword;
                }

                if ((_settings.Options & LexerOptions.IdentToLower) != 0)
                {
                    val = val.ToLower();
                }
            }

            return new Token(tokenType, val, GetTokenText(), (int)id, _start, _position, _lineBegin, _lineNumber, _lineBegin, _lineNumber);
        }

        private Token GetNumberToken(char currentChar)
        {
            _bufBeg = _textPos;
            do
            {
                ReadNext();
                BufferAdd(currentChar);
                currentChar = PeekChar();
            }
            while (currentChar >= '0' && currentChar <= '9');

            string decimalSeparator = _settings.DecimalSeparator;
            if (SymbolIs(decimalSeparator))
            {
                int ln = decimalSeparator.Length;
                char ch = PeekChar(ln);
                if (ch >= '0' && ch <= '9')
                {
                    Skip(ln);
                    BufferAdd(decimalSeparator);
                    currentChar = ch;
                    do
                    {
                        ReadNext();
                        BufferAdd(currentChar);
                        currentChar = PeekChar();
                    } while (currentChar >= '0' && currentChar <= '9');
                }
            }

            if (char.IsLetter(currentChar))
            {
                do
                {
                    ReadNext();
                    BufferAdd(currentChar);
                    currentChar = PeekChar();

                } while ((currentChar >= '0' && currentChar <= '9') || currentChar == '-' || currentChar == '+' || Char.IsLetter(currentChar));

                string val = GetBufferValue(0);
                return new Token(TokenType.Number, val, GetTokenText(), 0, _start, _position, _lineBegin, _lineNumber, _lineBegin, _lineNumber);
            }
            else
            {
                string val = GetBufferValue(0);
                decimal decimalVal;
                long intVal;
                if (long.TryParse(val, out intVal))
                {
                    return new Token(TokenType.Integer, intVal, GetTokenText(), 0, _start, _position, _lineBegin, _lineNumber, _lineBegin, _lineNumber);
                }
                else if (decimal.TryParse(val, out decimalVal))
                {
                    return new Token(TokenType.Decimal, decimalVal, GetTokenText(), 0, _start, _position, _lineBegin, _lineNumber, _lineBegin, _lineNumber);
                }
                else
                {
                    return new Token(TokenType.Number, val, GetTokenText(), 0, _start, _position, _lineBegin, _lineNumber, _lineBegin, _lineNumber);
                }
            }
        }

        private Token GetPrefixedIdentifierToken()
        {
            ReadNext();
            _bufBeg = _textPos;

            char currentChar = PeekChar();
            if (Char.IsLetterOrDigit(currentChar) || currentChar == '_' || IsIdentChar(currentChar))
            {
                do
                {
                    ReadNext();
                    BufferAdd(currentChar);
                    currentChar = PeekChar();
                }
                while (Char.IsLetterOrDigit(currentChar) || currentChar == '_' || IsIdentChar(currentChar));
            }

            string val = GetBufferValue(0);
            if ((_settings.Options & LexerOptions.IdentToUpper) != 0)
            {
                val = val.ToUpper(_settings.CultureInfo);
            }
            else if ((_settings.Options & LexerOptions.IdentToLower) != 0)
            {
                val = val.ToLower(_settings.CultureInfo);
            }

            return new Token(TokenType.Identifier, val, GetTokenText(), 0, _start, _position, _lineBegin, _lineNumber, _lineBegin, _lineNumber);
        }

        private bool IsIdentChar(char currentChar)
        {
            char[] identChars = _settings.IdentChars;
            if (identChars != null)
            {
                int len = identChars.Length;
                for (int i = 0; i < len; i++)
                {
                    char ch = identChars[i];
                    if (currentChar == ch)
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        private char PeekChar()
        {
            if (_textPos < _textLen)
            {
                return _text[_textPos];
            }

            if (_textLen == BufferCapacity)
            {
                ReadCharBuffer();
                if (_textPos < _textLen)
                {
                    return _text[_textPos];
                }
            }

            return EndOfTextChar;
        }

        private char PeekChar(int ofs)
        {
            int i = _textPos + ofs;
            if (i < _textLen)
            {
                return _text[i];
            }

            if (_textLen == BufferCapacity)
            {
                ReadCharBuffer();
                ofs += _textPos;
                if (ofs < _textLen)
                {
                    return _text[ofs];
                }
            }

            return EndOfTextChar;
        }

        private string PeekSubstring(int count)
        {
            if (_textPos + count <= _textLen)
            {
                return _text.Substring(_textPos, count);
            }

            if (_textLen == BufferCapacity)
            {
                ReadCharBuffer();
            }

            int i = _textLen - _textPos;
            if (count <= i)
            {
                return _text.Substring(_textPos, count);
            }
            else
            {
                return _text.Substring(_textPos, i);
            }
        }

        private char NextChar()
        {
            if (_textPos < _textLen)
            {
                _position++;
                return _text[_textPos++];
            }

            if (_textLen == BufferCapacity)
            {
                ReadCharBuffer();
                if (_textPos < _textLen)
                {
                    _position++;
                    return _text[_textPos++];
                }
            }

            return EndOfTextChar;
        }

        private void ReadNext()
        {
            if (_textPos < _textLen)
            {
                _position++;
                _textPos++;
            }
            else
            {
                if (_textLen == BufferCapacity)
                {
                    ReadCharBuffer();
                    _position++;
                    _textPos++;
                }
            }
        }

        private bool NextSymbolIs(string s)
        {
            int ln = s.Length;
            if (_textLen - _textPos < ln && _textLen == BufferCapacity)
            {
                ReadCharBuffer();
            }

            if (_textLen - _textPos < ln || _text[_textPos] != s[0])
            {
                return false;
            }

            if (_settings.CompareInfo.Compare(_text, _textPos, ln, s, 0, ln, CompareOptions.None) == 0)
            {
                _position += ln;
                _textPos += ln;
                return true;
            }

            return false;
        }

        private bool SymbolIs(string s)
        {
            int ln = s.Length;
            if (_textLen - _textPos < ln && _textLen == BufferCapacity)
            {
                ReadCharBuffer();
            }

            if (_textLen - _textPos < ln || _text[_textPos] != s[0])
            {
                return false;
            }

            return (_settings.CompareInfo.Compare(_text, _textPos, ln, s, 0, ln, CompareOptions.None) == 0);
        }

        private void Skip(int ofs)
        {
            if (_textLen - _textPos < ofs && _textLen == BufferCapacity)
            {
                ReadCharBuffer();
            }

            int i = Math.Min(_textLen - _textPos, ofs);
            _position += i;
            _textPos += i;
        }

        private bool EndOfLine(char currentChar)
        {
            if (currentChar == '\r')
            {
                _endLineNumber++;
                _endLineBegin = _position;
                currentChar = PeekChar();
                if (currentChar == '\n')
                {
                    ReadNext();
                    BufferAdd(currentChar);
                    _endLineBegin = _position;
                }

                return true;
            }
            else if (currentChar == '\n')
            {
                _endLineNumber++;
                _endLineBegin = _position;

                return true;
            }

            return false;
        }

        private bool EndOfText()
        {
            if (_textPos < _textLen)
            {
                return false;
            }

            if (_textLen == BufferCapacity)
            {
                ReadCharBuffer();
                return _textPos >= _textLen;
            }

            return true;
        }

        private void BufferAdd(char currentChar)
        {
            if (_buffer != null)
            {
                _buffer.Append(currentChar);
            }
            else if (_bufBeg >= 0 && _textPos >= _textLen)
            {
                _buffer = new StringBuilder(_text, _bufBeg, _textPos - _bufBeg, BufferCapacity);
            }
        }

        private void BufferAdd(string str)
        {
            if (_buffer != null)
            {
                _buffer.Append(str);
            }
            else if (_bufBeg >= 0 && _textPos >= _textLen)
            {
                _buffer = new StringBuilder(_text, _bufBeg, _textPos - _bufBeg, BufferCapacity);
            }
        }

        private void EnsureBuffer(int ofs)
        {
            if (_buffer == null)
            {
                _buffer = new StringBuilder(_text, _bufBeg, _textPos - _bufBeg - ofs, BufferCapacity);
            }
            else
            {
                _buffer.Remove(_buffer.Length - ofs, ofs);
            }
        }

        private string GetBufferValue(int ofs)
        {
            if (_buffer != null)
            {
                return _buffer.ToString(0, _buffer.Length + ofs);
            }
            else
            {
                return _text.Substring(_bufBeg, _textPos - _bufBeg + ofs);
            }
        }

        private void ReadCharBuffer()
        {
            if (_reader == null)
            {
                return;
            }

            if (_tokenBuffer != null)
            {
                _tokenBuffer.Append(_text, 0, _textPos);
            }
            else if (_textBeg < _textPos && (_behavior & LexerBehavior.PersistTokenText) != 0)
            {
                _tokenBuffer = new StringBuilder(_text, _textBeg, _textPos - _textBeg, BufferCapacity);
            }
            else
            {
                _textBeg = 0;
            }

            char[] charBuffer = new char[BufferCapacity];
            if (_textPos < _textLen)
            {
                if (_textPos == 0)
                {
                    throw new ArgumentException("'BufferCapacity' too small.");
                }
                _textLen -= _textPos;
                _text.CopyTo(_textPos, charBuffer, 0, _textLen);
            }
            else
            {
                _textLen = 0;
            }

            _textLen += _reader.Read(charBuffer, _textLen, BufferCapacity - _textLen);
            _text = new string(charBuffer, 0, _textLen);
            _textPos = 0;
        }

        private string GetTokenText()
        {
            if (_tokenBuffer != null)
            {
                _tokenBuffer.Append(_text, 0, _textPos);
                return _tokenBuffer.ToString(0, _tokenBuffer.Length);
            }

            if ((_behavior & LexerBehavior.PersistTokenText) == 0)
            {
                return null;
            }
            else
            {
                return _text.Substring(_textBeg, _textPos - _textBeg);
            }
        }

        #endregion

        #region IEnumerable<Token> Members

        IEnumerator<Token> IEnumerable<Token>.GetEnumerator()
        {
            return this;
        }

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this;
        }

        #endregion

        #region IEnumerator Members

        object IEnumerator.Current
        {
            get
            {
                return _current;
            }
        }

        bool IEnumerator.MoveNext()
        {
            return GetNextToken().Type != TokenType.End;
        }

        #endregion

        #region IDisposable Members

        public void Dispose()
        {
            if (_reader != null)
            {
                _reader.Dispose();
            }
        }

        #endregion
    }

    public enum TokenType
    {
        Char,
        Symbol,
        Number,
        Decimal,
        Integer,
        Identifier,
        Keyword,
        QuotedString,
        WhiteSpace,
        EndOfLine,
        Comment,
        Start,
        End
    }

    [Flags]
    public enum LexerBehavior
    {
        SkipWhiteSpaces = 1,
        SkipComments = 2,
        PersistTokenText = 4,
        Default = PersistTokenText
    }

    [Flags]
    public enum LexerOptions
    {
        IdentIgnoreCase = 1,
        IdentToLower = 3,
        IdentToUpper = 5,
        IdentDoubleQuote = 8,
        StringEscaping = 16,
        StringDoubleQuote = 32,
        EndOfLineAsToken = 64
    }

    public sealed class Token
    {
        public readonly TokenType Type;
        public readonly object Value;
        public readonly string Text;
        public readonly int Id;
        public readonly int StartPosition;
        public readonly int EndPosition;
        public readonly int LineBegin;
        public readonly int LineNumber;
        public readonly int EndLineBegin;
        public readonly int EndLineNumber;

        public Token(TokenType type, object value, string text, int id, int startPosition, int endPosition, int lineBegin, int lineNumber, int endLineBegin, int endLineNumber)
        {
            Type = type;
            Value = value;
            Text = text;
            Id = id;
            StartPosition = startPosition;
            EndPosition = endPosition;
            LineBegin = lineBegin;
            LineNumber = lineNumber;
            EndLineBegin = endLineBegin;
            EndLineNumber = endLineNumber;
        }

        public int LinePosition
        {
            get
            {
                return StartPosition - LineBegin;
            }
        }

        public int EndLinePosition
        {
            get
            {
                return EndPosition - EndLineBegin;
            }
        }
    }

    public sealed class LexerSettings : ICloneable
    {
        public LexerOptions Options { get; set; }
        public IDictionary<string, int> Symbols { get; set; }
        public IDictionary<string, int> Keywords { get; set; }
        public CultureInfo CultureInfo { get; set; }
        public CompareInfo CompareInfo { get; set; }
        public char[] StringQuotes { get; set; }
        public char StringEscapeChar { get; set; }
        public char StringPrefix { get; set; }
        public char IdentQuote { get; set; }
        public char IdentQuoteBegin { get; set; }
        public char IdentQuoteEnd { get; set; }
        public char IdentPrefix { get; set; }
        public char[] IdentChars { get; set; }
        public string[] InlineComments { get; set; }
        public string CommentBegin { get; set; }
        public string CommentEnd { get; set; }
        public string DecimalSeparator { get; set; }

        public static LexerSettings Default
        {
            get
            {
                LexerSettings settings = new LexerSettings();
                settings.CultureInfo = CultureInfo.InvariantCulture;
                settings.CompareInfo = CultureInfo.InvariantCulture.CompareInfo;
                settings.DecimalSeparator = ".";
                settings.Options = LexerOptions.IdentIgnoreCase | LexerOptions.StringDoubleQuote;
                settings.StringQuotes = new char[] { '\"', '\'' };
                settings.InlineComments = new string[] { "//" };
                settings.CommentBegin = "/*";
                settings.CommentEnd = "*/";
                settings.StringEscapeChar = '\\';
                settings.StringPrefix = '@';
                settings.IdentQuote = '\0';
                settings.IdentQuoteBegin = '\0';
                settings.IdentQuoteEnd = '\0';

                return settings;
            }
        }

        #region ICloneable Members

        object ICloneable.Clone()
        {
            return Clone();
        }

        public LexerSettings Clone()
        {
            LexerSettings settings = (LexerSettings)MemberwiseClone();

            if (settings.CultureInfo == null)
            {
                settings.CultureInfo = CultureInfo.InvariantCulture;
            }

            if (settings.CompareInfo == null)
            {
                settings.CompareInfo = settings.CultureInfo.CompareInfo;
            }

            if (string.IsNullOrEmpty(settings.DecimalSeparator))
            {
                settings.DecimalSeparator = settings.CultureInfo.NumberFormat.NumberDecimalSeparator;
            }

            if (settings.Symbols != null && settings.Symbols.Count > 0)
            {
                settings.Symbols = new Dictionary<string, int>(settings.Symbols);
            }
            else
            {
                settings.Symbols = null;
            }

            if (settings.Keywords != null && settings.Keywords.Count > 0)
            {
                bool ignoreCase = (settings.Options & LexerOptions.IdentIgnoreCase) != 0;
                settings.Keywords = new Dictionary<string, int>(settings.Keywords, StringComparer.Create(settings.CultureInfo, ignoreCase));
            }
            else
            {
                settings.Keywords = null;
            }

            if (settings.StringQuotes != null)
            {
                settings.StringQuotes = (char[])settings.StringQuotes.Clone();
            }

            if (settings.IdentChars != null)
            {
                settings.IdentChars = (char[])settings.IdentChars.Clone();
            }

            string[] inlineComments = settings.InlineComments;
            if (inlineComments != null)
            {
                int length = inlineComments.Length;
                int count = 0;
                for (int i = 0; i < length; i++)
                {
                    string inlineComment = inlineComments[i];
                    if (inlineComment == null)
                    {
                        continue;
                    }

                    if (i != count)
                    {
                        inlineComments[count] = inlineComment;
                    }

                    count++;
                }

                if (count == 0)
                {
                    settings.InlineComments = null;
                }
                else
                {
                    string[] arr = new string[count];
                    Array.Copy(inlineComments, 0, arr, 0, count);
                }
            }

            if (!string.IsNullOrEmpty(settings.CommentBegin) && string.IsNullOrEmpty(settings.CommentEnd))
            {
                settings.CommentEnd = settings.CommentBegin;
            }

            return settings;
        }

        #endregion
    }

    internal static class CommonLexem
    {
        public const int Start = 1;
        public const int End = 2;
    }
}