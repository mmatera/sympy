"""Symbolic primitives + unicode/ASCII abstraction for pretty.py"""

import sys
import unicodedata
import warnings
from string import ascii_lowercase, ascii_uppercase

from sympy.core.alphabets import greeks
from sympy.printing.conventions import split_super_sub
from sympy.utilities.exceptions import sympy_deprecation_warning

_USE_UNICODE = False
UNICODE_WARNINGS = ""


_xobj_ascii = {
    # vertical symbols
    #       (( ext, top, bot, mid ), c1)
    "(": (("|", "/", "\\"), "("),
    ")": (("|", "\\", "/"), ")"),
    # XXX this looks ugly
    #   '[':    (( '|', '-', '-' ), '['),
    #   ']':    (( '|', '-', '-' ), ']'),
    # XXX not so ugly :(
    "[": (("[", "[", "["), "["),
    "]": (("]", "]", "]"), "]"),
    "{": (("|", "/", "\\", "<"), "{"),
    "}": (("|", "\\", "/", ">"), "}"),
    "|": "|",
    "<": (("|", "/", "\\"), "<"),
    ">": (("|", "\\", "/"), ">"),
    "int": (" | ", "  /", "/  "),
    # horizontal objects
    "-": "-",
    "_": "_",
    # diagonal objects '\' & '/' ?
    "/": "/",
    "\\": "\\",
}

# {} '('  ->  (extension, start, end, middle) 1-character
_xobj_unicode = {}


# SYMBOLS

atoms_table = {}


# Bold
bold_unicode = {}


# Digits
digit_2txt = {
    "0": "ZERO",
    "1": "ONE",
    "2": "TWO",
    "3": "THREE",
    "4": "FOUR",
    "5": "FIVE",
    "6": "SIX",
    "7": "SEVEN",
    "8": "EIGHT",
    "9": "NINE",
}

# (p,q) -> symbol
frac = {}

# Greek letters
greek_letters = list(greeks)  # make a copy
# deal with Unicode's funny spelling of lambda
greek_letters[greek_letters.index("lambda")] = "lamda"
greek_unicode = {}

greek_bold_letters = list(greeks)  # make a copy, not strictly required here

# BOLD


# {}  greek letter -> (g,G)
greek_bold_unicode = {}


# Variable modifiers
# TODO: Make brackets adjust to height of contents
modifier_dict = {
    # Accents
    "mathring": lambda s: center_accent(s, "\N{COMBINING RING ABOVE}"),
    "ddddot": lambda s: center_accent(s, "\N{COMBINING FOUR DOTS ABOVE}"),
    "dddot": lambda s: center_accent(s, "\N{COMBINING THREE DOTS ABOVE}"),
    "ddot": lambda s: center_accent(s, "\N{COMBINING DIAERESIS}"),
    "dot": lambda s: center_accent(s, "\N{COMBINING DOT ABOVE}"),
    "check": lambda s: center_accent(s, "\N{COMBINING CARON}"),
    "breve": lambda s: center_accent(s, "\N{COMBINING BREVE}"),
    "acute": lambda s: center_accent(s, "\N{COMBINING ACUTE ACCENT}"),
    "grave": lambda s: center_accent(s, "\N{COMBINING GRAVE ACCENT}"),
    "tilde": lambda s: center_accent(s, "\N{COMBINING TILDE}"),
    "hat": lambda s: center_accent(s, "\N{COMBINING CIRCUMFLEX ACCENT}"),
    "bar": lambda s: center_accent(s, "\N{COMBINING OVERLINE}"),
    "vec": lambda s: center_accent(s, "\N{COMBINING RIGHT ARROW ABOVE}"),
    "prime": lambda s: s + "\N{PRIME}",
    "prm": lambda s: s + "\N{PRIME}",
    # # Faces -- these are here for some compatibility with latex printing
    # 'bold': lambda s: s,
    # 'bm': lambda s: s,
    # 'cal': lambda s: s,
    # 'scr': lambda s: s,
    # 'frak': lambda s: s,
    # Brackets
    "norm": lambda s: f"\N{DOUBLE VERTICAL LINE}{s}\N{DOUBLE VERTICAL LINE}",
    "avg": lambda s: (
        f"\N{MATHEMATICAL LEFT ANGLE BRACKET}{s}" "\N{MATHEMATICAL RIGHT ANGLE BRACKET}"
    ),
    "abs": lambda s: f"\N{VERTICAL LINE}{s}\N{VERTICAL LINE}",
    "mag": lambda s: f"\N{VERTICAL LINE}{s}\N{VERTICAL LINE}",
}


sub = {}  # symb -> subscript symbol
sup = {}  # symb -> superscript symbol

symb_2txt = {
    "+": "PLUS SIGN",
    "-": "MINUS",
    "=": "EQUALS SIGN",
    "(": "LEFT PARENTHESIS",
    ")": "RIGHT PARENTHESIS",
    "[": "LEFT SQUARE BRACKET",
    "]": "RIGHT SQUARE BRACKET",
    "{": "LEFT CURLY BRACKET",
    "}": "RIGHT CURLY BRACKET",
    # non-std
    "{}": "CURLY BRACKET",
    "sum": "SUMMATION",
    "int": "INTEGRAL",
}


# prefix conventions when constructing tables
# L   - LATIN     i
# G   - GREEK     beta
# D   - DIGIT     0
# S   - SYMBOL    +


# RADICAL
# n -> symbol
root = {}


# atom symbols
_xsym = {}


def _U(name):
    """
    Get a unicode character by name or, None if not found.

    This exists because older versions of Python use older unicode databases.
    """
    try:
        return unicodedata.lookup(name)
    except KeyError:
        global UNICODE_WARNINGS
        UNICODE_WARNINGS += f"No '{name}' in unicodedata\n"
        return None


def annotated(letter):
    """
    Return a stylised drawing of the letter ``letter``, together with
    information on how to put annotations (super- and subscripts to the
    left and to the right) on it.

    See pretty.py functions _print_meijerg, _print_hyper on how to use this
    information.
    """
    ucode_pics = {
        "F": (
            2,
            0,
            2,
            0,
            (
                "\N{BOX DRAWINGS LIGHT DOWN AND RIGHT}"
                "\N{BOX DRAWINGS LIGHT HORIZONTAL}\n"
                "\N{BOX DRAWINGS LIGHT VERTICAL AND RIGHT}"
                "\N{BOX DRAWINGS LIGHT HORIZONTAL}\n"
                "\N{BOX DRAWINGS LIGHT UP}"
            ),
        ),
        "G": (
            3,
            0,
            3,
            1,
            (
                "\N{BOX DRAWINGS LIGHT ARC DOWN AND RIGHT}"
                "\N{BOX DRAWINGS LIGHT HORIZONTAL}"
                "\N{BOX DRAWINGS LIGHT ARC DOWN AND LEFT}\n"
                "\N{BOX DRAWINGS LIGHT VERTICAL}\N{BOX DRAWINGS LIGHT RIGHT}"
                "\N{BOX DRAWINGS LIGHT DOWN AND LEFT}\n"
                "\N{BOX DRAWINGS LIGHT ARC UP AND RIGHT}"
                "\N{BOX DRAWINGS LIGHT HORIZONTAL}"
                "\N{BOX DRAWINGS LIGHT ARC UP AND LEFT}"
            ),
        ),
    }
    ascii_pics = {"F": (3, 0, 3, 0, " _\n|_\n|\n"), "G": (3, 0, 3, 1, " __\n/__\n\\_|")}

    if _USE_UNICODE:
        return ucode_pics[letter]
    return ascii_pics[letter]


def center(string, width, fillchar=" "):
    """Return a centered string of length determined by `line_width`
    that uses `fillchar` for padding.
    """
    left, right = center_pad(line_width(string), width, fillchar)
    return "".join([left, string, right])


def center_accent(string, accent):
    """
    Returns a string with accent inserted on the middle character. Useful to
    put combining accents on symbol names, including multi-character names.

    Parameters
    ==========

    string : string
        The string to place the accent in.
    accent : string
        The combining accent to insert

    References
    ==========

    .. [1] https://en.wikipedia.org/wiki/Combining_character
    .. [2] https://en.wikipedia.org/wiki/Combining_Diacritical_Marks

    """

    # Accent is placed on the previous character,
    # although it may not always look
    # like that depending on console
    midpoint = len(string) // 2 + 1
    firstpart = string[:midpoint]
    secondpart = string[midpoint:]
    return firstpart + accent + secondpart


def center_pad(wstring, wtarget, fillchar=" "):
    """
    Return the padding strings necessary to center a string of
    wstring characters wide in a wtarget wide space.

    The line_width wstring should always be less or equal to wtarget
    or else a ValueError will be raised.
    """
    if wstring > wtarget:
        raise ValueError("not enough space for string")
    wdelta = wtarget - wstring

    wleft = wdelta // 2  # favor left '1 '
    wright = wdelta - wleft

    left = fillchar * wleft
    right = fillchar * wright

    return left, right


def hobj(symb, width):
    """Construct horizontal object of a given width

    see: xobj
    """
    return "".join(xobj(symb, width))


def init():
    """Initialize variables"""

    # GREEK
    def g(letter):
        """Unicode character for Greek small letters"""
        return _U(f"GREEK SMALL LETTER {letter.upper()}")

    def G(letter):
        """Unicode character for Greek Capital letters"""
        return _U(f"GREEK CAPITAL LETTER {letter.upper()}")

    def b(letter):
        """Bold small letters"""
        return _U(f"MATHEMATICAL BOLD SMALL {letter.upper()}")

    def B(letter):
        """Bold Capital letters"""
        return _U(f"MATHEMATICAL BOLD CAPITAL {letter.upper()}")

    # GREEK BOLD

    def gb(letter):
        """Mathematical Bold small symbols"""
        return _U(f"MATHEMATICAL BOLD SMALL {letter.upper()}")

    def GB(letter):
        """Mathematical Bold Capital symbols"""
        return _U(f"MATHEMATICAL BOLD CAPITAL  {letter.upper()}")

    # deal with Unicode's funny spelling of lambda

    # SUBSCRIPT & SUPERSCRIPT

    def LSUB(letter):
        """Latin Subscript small letters"""
        return _U(f"LATIN SUBSCRIPT SMALL LETTER {letter.upper()}")

    def GSUB(letter):
        """Greek Subscript small letters"""
        return _U(f"GREEK SUBSCRIPT SMALL LETTER {letter.upper()}")

    def DSUB(digit):
        """Subscript digit"""
        return _U(f"SUBSCRIPT {digit_2txt[digit]}")

    def SSUB(symb):
        """Subscript symbol"""
        return _U(f"SUBSCRIPT {symb_2txt[symb]}")

    def LSUP(letter):
        """Superscript small letter"""
        return _U(f"SUPERSCRIPT LATIN SMALL LETTER {letter.upper()}")

    def DSUP(digit):
        """Superscript digit"""
        return _U(f"SUPERSCRIPT {digit_2txt[digit]}")

    def SSUP(symb):
        """Superscript symbol"""
        return _U(f"SUPERSCRIPT {symb_2txt[symb]}")

    # VERTICAL OBJECTS

    def HUP(symb):
        """Upper hook"""
        return _U(f"{symb_2txt[symb]} UPPER HOOK")

    def CUP(symb):
        """Upper corner"""
        return _U(f"{symb_2txt[symb]} UPPER CORNER")

    def MID(symb):
        """Middle piece"""
        return _U(f"{symb_2txt[symb]} MIDDLE PIECE")

    def EXT(symb):
        """Extension piece"""
        return _U(f"{symb_2txt[symb]} EXTENSION")

    def HLO(symb):
        """Lower hook"""
        return _U(f"{symb_2txt[symb]} LOWER HOOK")

    def CLO(symb):
        """Lower corner"""
        return _U(f"{symb_2txt[symb]} LOWER CORNER")

    def TOP(symb):
        """Top symbol"""
        return _U(f"{symb_2txt[symb]} TOP")

    def BOT(symb):
        """Bottom"""
        return _U(f"{symb_2txt[symb]} BOTTOM")

    # RATIONAL
    def VF(txt):
        """Vulgar fractions"""
        return _U(f"VULGAR FRACTION {txt}")

    # latin subscripts
    for letter in "aeioruvxhklmnpst":
        sub[letter] = LSUB(letter)

    for letter in "in":
        sup[letter] = LSUP(letter)

    for g_l in ["beta", "gamma", "rho", "phi", "chi"]:
        sub[g_l] = GSUB(g_l)

    for dig in [str(i) for i in range(10)]:
        sub[dig] = DSUB(dig)
        sup[dig] = DSUP(dig)

    for sign in "+-=()":
        sub[sign] = SSUB(sign)
        sup[sign] = SSUP(sign)

    _xsym.update(
        {
            "==": ("=", "="),
            "<": ("<", "<"),
            ">": (">", ">"),
            "<=": ("<=", _U("LESS-THAN OR EQUAL TO")),
            ">=": (">=", _U("GREATER-THAN OR EQUAL TO")),
            "!=": ("!=", _U("NOT EQUAL TO")),
            ":=": (":=", ":="),
            "+=": ("+=", "+="),
            "-=": ("-=", "-="),
            "*=": ("*=", "*="),
            "/=": ("/=", "/="),
            "%=": ("%=", "%="),
            "*": ("*", _U("DOT OPERATOR")),
            "-->": (
                "-->",
                (
                    _U("EM DASH") + _U("EM DASH") + _U("BLACK RIGHT-POINTING TRIANGLE")
                    if _U("EM DASH") and _U("BLACK RIGHT-POINTING TRIANGLE")
                    else None
                ),
            ),
            "==>": (
                "==>",
                (
                    _U("BOX DRAWINGS DOUBLE HORIZONTAL")
                    + _U("BOX DRAWINGS DOUBLE HORIZONTAL")
                    + _U("BLACK RIGHT-POINTING TRIANGLE")
                    if _U("BOX DRAWINGS DOUBLE HORIZONTAL")
                    and _U("BOX DRAWINGS DOUBLE HORIZONTAL")
                    and _U("BLACK RIGHT-POINTING TRIANGLE")
                    else None
                ),
            ),
            ".": ("*", _U("RING OPERATOR")),
        }
    )
    _xobj_unicode.update(
        {
            # vertical symbols
            #                       (( ext, top, bot, mid ), c1)
            "(": ((EXT("("), HUP("("), HLO("(")), "("),
            ")": ((EXT(")"), HUP(")"), HLO(")")), ")"),
            "[": ((EXT("["), CUP("["), CLO("[")), "["),
            "]": ((EXT("]"), CUP("]"), CLO("]")), "]"),
            "{": ((EXT("{}"), HUP("{"), HLO("{"), MID("{")), "{"),
            "}": ((EXT("{}"), HUP("}"), HLO("}"), MID("}")), "}"),
            "|": _U("BOX DRAWINGS LIGHT VERTICAL"),
            "Tee": _U("BOX DRAWINGS LIGHT UP AND HORIZONTAL"),
            "UpTack": _U("BOX DRAWINGS LIGHT DOWN AND HORIZONTAL"),
            "corner_up_centre" "(_ext": _U("LEFT PARENTHESIS EXTENSION"),
            ")_ext": _U("RIGHT PARENTHESIS EXTENSION"),
            "(_lower_hook": _U("LEFT PARENTHESIS LOWER HOOK"),
            ")_lower_hook": _U("RIGHT PARENTHESIS LOWER HOOK"),
            "(_upper_hook": _U("LEFT PARENTHESIS UPPER HOOK"),
            ")_upper_hook": _U("RIGHT PARENTHESIS UPPER HOOK"),
            "<": (
                (
                    _U("BOX DRAWINGS LIGHT VERTICAL"),
                    _U("BOX DRAWINGS LIGHT DIAGONAL UPPER RIGHT TO LOWER LEFT"),
                    _U("BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT"),
                ),
                "<",
            ),
            ">": (
                (
                    _U("BOX DRAWINGS LIGHT VERTICAL"),
                    _U("BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT"),
                    _U("BOX DRAWINGS LIGHT DIAGONAL UPPER RIGHT TO LOWER LEFT"),
                ),
                ">",
            ),
            "lfloor": ((EXT("["), EXT("["), CLO("[")), _U("LEFT FLOOR")),
            "rfloor": ((EXT("]"), EXT("]"), CLO("]")), _U("RIGHT FLOOR")),
            "lceil": ((EXT("["), CUP("["), EXT("[")), _U("LEFT CEILING")),
            "rceil": ((EXT("]"), CUP("]"), EXT("]")), _U("RIGHT CEILING")),
            "int": (
                (EXT("int"), _U("TOP HALF INTEGRAL"), _U("BOTTOM HALF INTEGRAL")),
                _U("INTEGRAL"),
            ),
            "sum": (
                (
                    _U("BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT"),
                    "_",
                    _U("OVERLINE"),
                    _U("BOX DRAWINGS LIGHT DIAGONAL UPPER RIGHT TO LOWER LEFT"),
                ),
                _U("N-ARY SUMMATION"),
            ),
            # horizontal objects
            # '-':   '-',
            "-": _U("BOX DRAWINGS LIGHT HORIZONTAL"),
            "_": _U("LOW LINE"),
            # We used to use this, but LOW LINE looks better for roots, as it's a
            # little lower (i.e., it lines up with the / perfectly.
            # But perhaps this one would still be wanted for some cases?
            # '_':    _U('HORIZONTAL SCAN LINE-9'),
            # diagonal objects '\' & '/' ?
            "/": _U("BOX DRAWINGS LIGHT DIAGONAL UPPER RIGHT TO LOWER LEFT"),
            "\\": _U("BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT"),
        }
    )

    atoms_table.update(
        {
            # class                    how-to-display
            "Exp1": _U("SCRIPT SMALL E"),
            "Pi": _U("GREEK SMALL LETTER PI"),
            "Infinity": _U("INFINITY"),
            # XXX what to do here
            "NegativeInfinity": _U("INFINITY") and ("-" + _U("INFINITY")),
            # 'ImaginaryUnit':          _U('GREEK SMALL LETTER IOTA'),
            # 'ImaginaryUnit':          _U('MATHEMATICAL ITALIC SMALL I'),
            "ImaginaryUnit": _U("DOUBLE-STRUCK ITALIC SMALL I"),
            "EmptySet": _U("EMPTY SET"),
            "Naturals": _U("DOUBLE-STRUCK CAPITAL N"),
            "Naturals0": (
                _U("DOUBLE-STRUCK CAPITAL N")
                and (_U("DOUBLE-STRUCK CAPITAL N") + _U("SUBSCRIPT ZERO"))
            ),
            "Integers": _U("DOUBLE-STRUCK CAPITAL Z"),
            "Rationals": _U("DOUBLE-STRUCK CAPITAL Q"),
            "Reals": _U("DOUBLE-STRUCK CAPITAL R"),
            "Complexes": _U("DOUBLE-STRUCK CAPITAL C"),
            "Universe": _U("MATHEMATICAL DOUBLE-STRUCK CAPITAL U"),
            "IdentityMatrix": _U("MATHEMATICAL DOUBLE-STRUCK CAPITAL I"),
            "ZeroMatrix": _U("MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO"),
            "OneMatrix": _U("MATHEMATICAL DOUBLE-STRUCK DIGIT ONE"),
            "Differential": _U("DOUBLE-STRUCK ITALIC SMALL D"),
            "Union": _U("UNION"),
            "ElementOf": _U("ELEMENT OF"),
            "SmallElementOf": _U("SMALL ELEMENT OF"),
            "SymmetricDifference": _U("INCREMENT"),
            "Intersection": _U("INTERSECTION"),
            "Ring": _U("RING OPERATOR"),
            "Multiplication": _U("MULTIPLICATION SIGN"),
            "TensorProduct": _U("N-ARY CIRCLED TIMES OPERATOR"),
            "Dots": _U("HORIZONTAL ELLIPSIS"),
            "Modifier Letter Low Ring": _U("Modifier Letter Low Ring"),
            "EmptySequence": "EmptySequence",
            "SuperscriptPlus": _U("SUPERSCRIPT PLUS SIGN"),
            "SuperscriptMinus": _U("SUPERSCRIPT MINUS"),
            "Dagger": _U("DAGGER"),
            "Degree": _U("DEGREE SIGN"),
            # Logic Symbols
            "And": _U("LOGICAL AND"),
            "Or": _U("LOGICAL OR"),
            "Not": _U("NOT SIGN"),
            "Nor": _U("NOR"),
            "Nand": _U("NAND"),
            "Xor": _U("XOR"),
            "Equiv": _U("LEFT RIGHT DOUBLE ARROW"),
            "NotEquiv": _U("LEFT RIGHT DOUBLE ARROW WITH STROKE"),
            "Implies": _U("LEFT RIGHT DOUBLE ARROW"),
            "NotImplies": _U("LEFT RIGHT DOUBLE ARROW WITH STROKE"),
            "Arrow": _U("RIGHTWARDS ARROW"),
            "ArrowFromBar": _U("RIGHTWARDS ARROW FROM BAR"),
            "NotArrow": _U("RIGHTWARDS ARROW WITH STROKE"),
            "Tautology": _U("BOX DRAWINGS LIGHT UP AND HORIZONTAL"),
            "Contradiction": _U("BOX DRAWINGS LIGHT DOWN AND HORIZONTAL"),
        }
    )

    frac.update(
        {
            (1, 2): VF("ONE HALF"),
            (1, 3): VF("ONE THIRD"),
            (2, 3): VF("TWO THIRDS"),
            (1, 4): VF("ONE QUARTER"),
            (3, 4): VF("THREE QUARTERS"),
            (1, 5): VF("ONE FIFTH"),
            (2, 5): VF("TWO FIFTHS"),
            (3, 5): VF("THREE FIFTHS"),
            (4, 5): VF("FOUR FIFTHS"),
            (1, 6): VF("ONE SIXTH"),
            (5, 6): VF("FIVE SIXTHS"),
            (1, 8): VF("ONE EIGHTH"),
            (3, 8): VF("THREE EIGHTHS"),
            (5, 8): VF("FIVE EIGHTHS"),
            (7, 8): VF("SEVEN EIGHTHS"),
        }
    )

    bold_unicode.update((letter, B(letter)) for letter in ascii_uppercase)
    bold_unicode.update({letter: b(letter) for letter in ascii_lowercase})

    # {}  greek letter -> (g,G)
    greek_unicode.update({L: g(L) for L in greek_letters})

    greek_unicode.update((L[0].upper() + L[1:], G(L)) for L in greek_letters)

    # aliases
    greek_unicode["lambda"] = greek_unicode["lamda"]
    greek_unicode["Lambda"] = greek_unicode["Lamda"]
    greek_unicode["varsigma"] = "\N{GREEK SMALL LETTER FINAL SIGMA}"
    greek_bold_unicode["lambda"] = greek_unicode["lamda"]
    greek_bold_unicode["Lambda"] = greek_unicode["Lamda"]
    greek_bold_letters[greek_bold_letters.index("lambda")] = "lamda"
    greek_bold_unicode.update({L: g(L) for L in greek_bold_letters})
    greek_bold_unicode.update((L[0].upper() + L[1:], G(L)) for L in greek_bold_letters)
    greek_bold_unicode["varsigma"] = "\N{MATHEMATICAL BOLD SMALL FINAL SIGMA}"
    root.update(
        {
            2: _U("SQUARE ROOT"),  # _U('RADICAL SYMBOL BOTTOM')
            3: _U("CUBE ROOT"),
            4: _U("FOURTH ROOT"),
        }
    )
    pretty_try_use_unicode()


def is_combining(sym):
    """Check whether symbol is a unicode modifier."""

    return ord(sym) in _remove_combining


def is_subscriptable_in_unicode(subscript):
    """
    Checks whether a string is subscriptable in unicode or not.

    Parameters
    ==========

    subscript: the string which needs to be checked

    Examples
    ========

    >>> from sympy.printing.pretty.pretty_symbology import is_subscriptable_in_unicode
    >>> is_subscriptable_in_unicode('abc')
    False
    >>> is_subscriptable_in_unicode('123')
    True

    """
    return all(character in sub for character in subscript)


def line_width(line):
    """Unicode combining symbols (modifiers) are not ever displayed as
    separate symbols and thus should not be counted
    """
    return len(line.translate(_remove_combining))


def pretty_atom(atom_name, default=None, printer=None):
    """return pretty representation of an atom"""
    if _USE_UNICODE:
        if (
            printer is not None
            and atom_name == "ImaginaryUnit"
            and printer._settings["imaginary_unit"] == "j"
        ):
            return _U("DOUBLE-STRUCK ITALIC SMALL J")

        return atoms_table[atom_name]

    if default is not None:
        return default

    raise KeyError("only unicode")  # send it default printer


def pretty_symbol(symb_name, bold_name=False):
    """return pretty representation of a symbol"""
    # let's split symb_name into symbol + index
    # UC: beta1
    # UC: f_beta

    if not _USE_UNICODE:
        return symb_name

    name, sups, subs = split_super_sub(symb_name)

    def translate(char_str, bold_name):
        if bold_name:
            gG = greek_bold_unicode.get(char_str)
        else:
            gG = greek_unicode.get(char_str)
        if gG is not None:
            return gG
        for key in sorted(modifier_dict.keys(), key=len, reverse=True):
            if char_str.lower().endswith(key) and len(char_str) > len(key):
                return modifier_dict[key](translate(char_str[: -len(key)], bold_name))
        if bold_name:
            return "".join([bold_unicode[c] for c in char_str])
        return char_str

    name = translate(name, bold_name)

    # Let's prettify sups/subs. If it fails at one of them, pretty
    # sups/subs are not used at all.
    def pretty_list(lst, mapping):
        result = []
        for char_str in lst:
            pretty = mapping.get(char_str)
            if pretty is None:
                try:  # match by separate characters
                    pretty = "".join([mapping[c] for c in char_str])
                except (TypeError, KeyError):
                    return None
            result.append(pretty)
        return result

    pretty_sups = pretty_list(sups, sup)
    if pretty_sups is not None:
        pretty_subs = pretty_list(subs, sub)
    else:
        pretty_subs = None

    # glue the results into one string
    if pretty_subs is None:  # nice formatting of sups/subs did not work
        if subs:
            name += "_" + "_".join([translate(s, bold_name) for s in subs])
        if sups:
            name += "__" + "__".join([translate(s, bold_name) for s in sups])
        return name

    sups_result = " ".join(pretty_sups)
    subs_result = " ".join(pretty_subs)

    return "".join([name, sups_result, subs_result])


def pretty_use_unicode(flag=None):
    """Set whether pretty-printer should use unicode by default"""
    global _USE_UNICODE
    global UNICODE_WARNINGS
    if flag is None:
        return _USE_UNICODE

    if flag and UNICODE_WARNINGS:
        # print warnings (if any) on first unicode usage
        warnings.warn(UNICODE_WARNINGS)
        UNICODE_WARNINGS = ""

    use_unicode_prev = _USE_UNICODE
    _USE_UNICODE = flag
    return use_unicode_prev


def pretty_try_use_unicode():
    """See if unicode output is available and leverage it if possible"""

    encoding = getattr(sys.stdout, "encoding", None)

    # this happens when e.g. stdout is redirected through a pipe, or is
    # e.g. a cStringIO.StringO
    if encoding is None:
        return  # sys.stdout has no encoding

    symbols = []

    # see if we can represent greek alphabet
    symbols += greek_unicode.values()

    # and atoms
    symbols += atoms_table.values()

    for symbol in symbols:
        if symbol is None:
            return  # common symbols not present!

        try:
            symbol.encode(encoding)
        except UnicodeEncodeError:
            return

    # all the characters were present and encodable
    pretty_use_unicode(True)


def vobj(symb, height):
    """Construct vertical object of a given height

    see: xobj
    """
    return "\n".join(xobj(symb, height))


def xobj(symb, length):
    """Construct spatial object of given length.

    return: [] of equal-length strings
    """

    if length <= 0:
        raise ValueError("Length should be greater than 0")

    # TODO robustify when no unicodedat available
    if _USE_UNICODE:
        _xobj = _xobj_unicode
    else:
        _xobj = _xobj_ascii

    vinfo = _xobj[symb]

    c_1 = top = bot = mid = None

    if not isinstance(vinfo, tuple):  # 1 entry
        ext = vinfo
    else:
        if isinstance(vinfo[0], tuple):  # (vlong), c1
            vlong = vinfo[0]
            c_1 = vinfo[1]
        else:  # (vlong), c1
            vlong = vinfo

        ext = vlong[0]

        try:
            top = vlong[1]
            bot = vlong[2]
            mid = vlong[3]
        except IndexError:
            pass

    if c_1 is None:
        c_1 = ext
    if top is None:
        top = ext
    if bot is None:
        bot = ext
    if mid is not None:
        if (length % 2) == 0:
            # even height, but we have to print it somehow anyway...
            # XXX is it ok?
            length += 1

    else:
        mid = ext

    if length == 1:
        return c_1

    res = []
    next_ = (length - 2) // 2
    nmid = (length - 2) - next_ * 2

    res += [top]
    res += [ext] * next_
    res += [mid] * nmid
    res += [ext] * next_
    res += [bot]

    return res


def xstr(*args):
    """Support for unicode strings in old Python version"""
    sympy_deprecation_warning(
        """
        The sympy.printing.pretty.pretty_symbology.xstr() function is
        deprecated. Use str() instead.
        """,
        deprecated_since_version="1.7",
        active_deprecations_target="deprecated-pretty-printing-functions",
    )
    return str(*args)


def xsym(sym):
    """get symbology for a 'character'"""
    op_ = _xsym[sym]

    if _USE_UNICODE:
        return op_[1]
    return op_[0]


_remove_combining = dict.fromkeys(
    list(
        range(
            ord("\N{COMBINING GRAVE ACCENT}"), ord("\N{COMBINING LATIN SMALL LETTER X}")
        )
    )
    + list(
        range(
            ord("\N{COMBINING LEFT HARPOON ABOVE}"), ord("\N{COMBINING ASTERISK ABOVE}")
        )
    )
)


__all__ = [
    "greek_unicode",
    "sub",
    "sup",
    "xsym",
    "vobj",
    "hobj",
    "pretty_symbol",
    "annotated",
    "center_pad",
    "center",
]


init()

# Deprecated
U = _U
