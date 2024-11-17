"""
Prettyprinter by Jurjen Bos.

Classes used to render mathematical expressions as text-based pictures.
"""

import shutil
from typing import List

from sympy.utilities.exceptions import sympy_deprecation_warning

from .pretty_symbology import (
    center,
    hobj,
    line_width,
    pretty_use_unicode,
    vobj,
    xobj,
    xsym,
)

_GLOBAL_WRAP_LINE = None


class StringPict:
    """An ASCII picture.
    The pictures are represented as a list of equal length strings.
    """

    # special value for StringPict.below
    LINE = "line"

    def __init__(self, text: str, baseline: int = 0):
        """Initialize from string.
        Multiline strings are centered.
        """
        assert isinstance(text, str), f'type {type(text)} is not str'
        assert isinstance(baseline, int)
        self.text = text
        # picture is a string that just can be printed
        self.picture = StringPict.equal_lengths(text.splitlines())
        # baseline is the line number of the "base line"
        self.baseline = baseline
        self.binding = None

    def __eq__(self, other):
        if isinstance(other, str):
            return "\n".join(self.picture) == other
        if isinstance(other, StringPict):
            return other.picture == self.picture
        return False

    def __hash__(self):
        return super().__hash__()

    def __str__(self):
        return "\n".join(self.picture)

    def __repr__(self):
        return "StringPict(%r,%d)" % ("\n".join(self.picture), self.baseline)

    def __getitem__(self, index):
        return self.picture[index]

    def __len__(self):
        return len(self.text)

    def above(self, *args, align="c"):
        """
        Put pictures above this picture.

        Returns string, baseline arguments for StringPict.
        Baseline is baseline of bottom picture.
        the horizontal alignment is controlled by the keyword
        parameter `align`. See StringPict.below() for details and
        examples.
        """
        string, baseline = StringPict.stack(*(args + (self,)), align=align)
        baseline = len(string.splitlines()) - self.height() + self.baseline
        return self.__class__(string, baseline)

    def below(self, *args, align="c"):
        """Put pictures under this picture.
        Returns string, baseline arguments for StringPict.
        Baseline is baseline of top picture

        Examples
        ========

        Let' s consider first how to draw a fraction::

            >>> from sympy.printing.pretty.stringpict import StringPict
            >>> print(StringPict("x+3").below(
            ...       StringPict.LINE, '3')) #doctest: +NORMALIZE_WHITESPACE
            x+3
            ---
             3

        The optional argument `align` controls the alignment.
        The default value is `'c'`. For `align='l'`,
        the text is left aligned::

            >>> print(StringPict("Humpty Dumpty").below(
            ...       StringPict("falls off the wall..."), align="l"))
            Humpty Dumpty
            falls off the wall...

        and for `align='r'`, the elements are right aligned::

            >>> print(StringPict("Humpty Dumpty").below(
            ...       StringPict("falls off the wall..."), align="r"))
                    Humpty Dumpty
            falls off the wall...

        """
        text, _ = StringPict.stack(self, *args, align=align)
        return self.__class__(text, self.baseline)

    #    @staticmethod
    #    def equalLengths(lines: List[str]):
    #        return StringPict.equal_lengths(lines)

    @staticmethod
    def equal_lengths(lines: List[str]):
        """
        Return a new list of lines
        all with the same length, and
        centered.
        """
        # empty lines
        if not lines:
            return [""]

        width = max(line_width(line) for line in lines)
        return [center(line, width) for line in lines]

    def height(self):
        """The height of the picture in characters."""
        return len(self.picture)

    def join(self, *args, align=None):
        """
        Concatenate any number of StringsPicts.

        The StringPict whose method is called is inserted in
        between each given arguments.
        The result is returned as a new StringPict

        Examples
        ========

        >>> from sympy.printing.pretty.stringpict import StringPict, PrettyForm
        >>> from sympy.printing.pretty.pretty_symbology import vobj
        >>> sep = StringPict(", ")
        >>> fractions = [PrettyForm(str(i))/PrettyForm("6") for i in range(1, 6)]
        >>> print(sep.join(*fractions))
        1  2  3  4  5
        -, -, -, -, -
        6  6  6  6  6

        Using a separator with a larger height

        >>> sep = StringPict(vobj("|", 3)).left(" ").right(" ")
        >>> print(sep.join(*fractions, align="t"))
        1 | 2 | 3 | 4 | 5
        - | - | - | - | -
        6 | 6 | 6 | 6 | 6

        """
        if len(args) == 0:
            return StringPict("")
        result, *rest = args
        if isinstance(result, str):
            result = StringPict(result)
        for elem in rest:
            result = result.right(self, elem, align=align)
        return result

    def left(self, *args, align=""):
        """
        Put pictures (left to right) at left.
        Returns string, baseline arguments for StringPict.

        The parameter `align` controls the relative alignments
        of the elements. By default (`align=""`) aligns the
        elements regarding the base line.

        The other possible values for the `align` parameter
        are

            - "t": tops are aligned
            - "c": centers are aligned
            - "b": bottoms are aligned

        In all the cases, the resulting baseline is localized
        on to the `self` baseline.

        Examples
        ========

        >>> from sympy.printing.pretty.stringpict import StringPict
        >>> StringPict('a\\n-\\nb', 1).left(
        ...     "numerator-> ",align="t").right(" <-denominator",
        ...      align="b").parenthesis("","}").right(" fraction")
        numerator-> a              \\
                    -              > fraction
                    b <-denominator/
        """

        if align == "":
            return self.__class__(*StringPict.next(*(args + (self,))))
        return self.__class__(
            *StringPict.next(*self.v_align(*(args + (self,)), align=align))
        )

    def leftslash(self):
        """Precede object by a slash of the proper size."""
        # XXX not used anywhere ?
        height = max(self.baseline, self.height() - 1 - self.baseline) * 2 + 1
        slash = "\n".join(
            " " * (height - i - 1) + xobj("/", 1) + " " * i for i in range(height)
        )
        return self.left(StringPict(slash, height // 2))

    @staticmethod
    def next(*args):
        """Put a string of StringPicts next to each other.
        Returns string, baseline arguments for StringPict.
        """
        # convert everything to StringPicts
        objects = []
        for arg in args:
            if isinstance(arg, str):
                arg = StringPict(arg)
            objects.append(arg)

        # make a list of pictures, with equal height and baseline
        new_baseline = max((obj.baseline for obj in objects))
        new_height_below_baseline = max(obj.height() - obj.baseline for obj in objects)
        new_height = new_baseline + new_height_below_baseline

        pictures = []
        for obj in objects:
            one_empty_line = [" " * obj.width()]
            base_padding = new_baseline - obj.baseline
            total_padding = new_height - obj.height()
            pictures.append(
                one_empty_line * base_padding
                + obj.picture
                + one_empty_line * (total_padding - base_padding)
            )

        result = ["".join(lines) for lines in zip(*pictures)]
        return "\n".join(result), new_baseline

    def parens(self, left="(", right=")", ifascii_nougly=False):
        """Put parentheses around self.
        Returns string, baseline arguments for StringPict.

        left or right can be None or empty string which means 'no paren from
        that side'.
        """

        # TODO: Add a deprecation warning in favor of
        # `self.parenthesis(...)`?

        height: int = self.height()
        baseline: int = self.baseline

        # XXX this is a hack -- ascii parens are ugly!
        if ifascii_nougly and not pretty_use_unicode():
            height = 1
            baseline = 0

        res = self

        if left:
            lparen = StringPict(vobj(left, height), baseline=baseline)
            res = lparen.right(self)
        if right:
            rparen = StringPict(vobj(right, height), baseline=baseline)
            res = res.right(rparen)

        return "\n".join(res.picture), res.baseline

    def parenthesis(self, left="(", right=")", ifascii_nougly=False):
        """Returns a new object of the same type than self,
        sorrounded by parenthesis of the type specified by
        the arguments `left` and `right`.

        `left` or `right` can be None or empty string which means
        'no paren from that side'
        """
        height = self.height()
        baseline = self.baseline

        # XXX this is a hack -- ascii parens are ugly!
        if ifascii_nougly and not pretty_use_unicode():
            height = 1
            baseline = 0

        res = self

        if left:
            lparen = StringPict(vobj(left, height), baseline=baseline)
            res = lparen.right(self)
        if right:
            rparen = StringPict(vobj(right, height), baseline=baseline)
            res = res.right(rparen)

        return self.__class__("\n".join(res.picture), res.baseline)

    def render(self, *args, **kwargs):
        """Return the str form of self.

        Unless the argument line_break is set to False, it will
        break the expression in a form that can be printed
        on the terminal without being broken up.
        """
        assert len(args) == 0

        if _GLOBAL_WRAP_LINE is not None:
            kwargs["wrap_line"] = _GLOBAL_WRAP_LINE

        if kwargs["wrap_line"] is False:
            return "\n".join(self.picture)

        if kwargs["num_columns"] is not None:
            # Read the argument num_columns if it is not None
            ncols = kwargs["num_columns"]
        else:
            # Attempt to get a terminal width
            ncols = self.terminal_width()

        if ncols <= 0:
            ncols = 80

        # If smaller than the terminal width, no need to correct
        if self.width() <= ncols:
            return type(self.picture[0])(self)

        # """
        # Break long-lines in a visually pleasing format.
        # without overflow indicators | with overflow indicators
        # |   2  2        3     |     |   2  2        3    ↪|
        # |6*x *y  + 4*x*y  +   |     |6*x *y  + 4*x*y  +  ↪|
        # |                     |     |                     |
        # |     3    4    4     |     |↪      3    4    4   |
        # |4*y*x  + x  + y      |     |↪ 4*y*x  + x  + y    |
        # |a*c*e + a*c*f + a*d  |     |a*c*e + a*c*f + a*d ↪|
        # |*e + a*d*f + b*c*e   |     |                     |
        # |+ b*c*f + b*d*e + b  |     |↪ *e + a*d*f + b*c* ↪|
        # |*d*f                 |     |                     |
        # |                     |     |↪ e + b*c*f + b*d*e ↪|
        # |                     |     |                     |
        # |                     |     |↪ + b*d*f            |
        # """

        overflow_first = ""
        if kwargs["use_unicode"] or pretty_use_unicode():
            overflow_start = "\N{RIGHTWARDS ARROW WITH HOOK} "
            overflow_end = " \N{RIGHTWARDS ARROW WITH HOOK}"
        else:
            overflow_start = "> "
            overflow_end = " >"

        def chunks(line):
            """Yields consecutive chunks of line_width ncols"""
            prefix = overflow_first
            width, start = line_width(prefix + overflow_end), 0
            for i, x_char in enumerate(line):
                width_x = line_width(x_char)
                # Only flush the screen when the current character overflows.
                # This way, combining marks can be appended
                # even when width == ncols.
                if width + width_x > ncols:
                    yield prefix + line[start:i] + overflow_end
                    prefix = overflow_start
                    width, start = line_width(prefix + overflow_end), i
                width += width_x
            yield prefix + line[start:]

        # Concurrently assemble chunks of all lines into individual screens
        pictures = zip(*map(chunks, self.picture))

        # Join lines of each screen into sub-pictures
        pictures = ["\n".join(picture) for picture in pictures]

        # Add spacers between sub-pictures
        return "\n\n".join(pictures)

    def right(self, *args, align=""):
        r"""Put pictures next to this one.
        Returns string, baseline arguments for StringPict.
        (Multiline) strings are allowed, and are given a baseline of 0.

        Examples
        ========

        >>> from sympy.printing.pretty.stringpict import StringPict
        >>> print(StringPict("10").right(" + ",StringPict("1\r-\r2",1)))
             1
        10 + -
             2

        """

        if align is None or align == "":
            return self.__class__(*StringPict.next(self, *args))

        return self.__class__(
            *StringPict.next(*self.v_align(*((self,) + args), align=align))
        )

    def root(self, index=None):
        """
        Produce a nice root symbol.
        Produces ugly results for big n inserts.

        Examples
        ========

        >>> from sympy.printing.pretty.stringpict import StringPict, PrettyForm
        >>> print(StringPict("x+3").root().right(" + a"))
          _____
        \\/ x+3 + a

        >>> print(StringPict("x+3").root(StringPict("3")).right(" + a"))
        3 _____
        \\/ x+3 + a

        >>> print((PrettyForm("x")**StringPict("a")).root().right(" + a"))
           ____
          /  a
        \\/  x + a

        >>> print((PrettyForm("x")**StringPict("a")).root(StringPict("3")).right(" + a"))
           ____
        3 /  a
        \\/  x + a

        >>> print((PrettyForm("x+3")/PrettyForm("y")).root().right(" + a"))
            _____
           / x+3
          /  --- + a
        \\/   y

        >>> print((PrettyForm("x+3")/PrettyForm("y")).root(StringPict("3")).right(" + a"))
            _____
           / x+3
        3 /  --- + a
        \\/    y

        For indices with more than one line, use the Pow form:

        >>> print((PrettyForm("x+3")/PrettyForm("y")).root(
        ...       PrettyForm("3")/PrettyForm("5")).right(" + a"))
                 /3\\
             1 / |-|
                 \\5/
        /x+3\\
        |---|         + a
        \\ y /
        """
        # Decide if using a square root symbol or
        # an base - exponent form:
        if index is not None:
            if isinstance(index, str):
                index = index.ljust(2)
                index = StringPict(index)
            elif index.width() < 2:
                index = StringPict(str(index).ljust(2))
            if index.height() > 1:
                exponent = index.parenthesis().left(StringPict("1 / "), align="c")
                return self**exponent

        # put line over expression
        result = self.above(hobj("_", 2 + self.width()))
        # construct right half of root symbol
        height = self.height()
        bar_char: str = xobj("/", 1)
        root_sign = PrettyForm(xobj("\\", 1) + bar_char)
        if index is not None:
            root_sign = root_sign.above(index, align="r")
        if height > 1:
            slash = "\n".join(
                " " * (height - i - 2) + bar_char + " " * i for i in range(height - 1)
            )
            # TODO: To improve the use of the space, consider
            # using a vertical line instead '/', like
            #    -
            #   |x
            # 20|-
            #  \|2
            #
            # # remove the `.ljust.(2)` in `n` and
            # # replace the previous line by
            # _zZ = xobj('|', 1)
            # slash = "\n".join(height*[_zZ])
            #
            # but this requires to change many tests.
            slash = StringPict(" ").above(slash, align="l")
            root_sign = root_sign.right(slash, align="b")

        return result.left(root_sign, align="b")

    def subindex(self, sub_index):
        """Add a `subindex`.

        Examples
        ========

        >>> from sympy.printing.pretty.stringpict import StringPict
        >>> print( (StringPict("a").below("-").below(StringPict("b"))
        ...         ).parenthesis().subindex(StringPict("a=4")))
        /a\\
        |-|
        \\b/
            a=4
        """
        width_self = self.width()
        width_index = sub_index.width()
        extended_self = self.right(StringPict(width_index * " "))
        sub_index = sub_index.left(StringPict(width_self * " "))
        return extended_self.below(sub_index)

    def subsuperindices(self, sub_index, super_index):
        """
        Add  sub and super indices.

        Examples
        ========

        >>> from sympy.printing.pretty.stringpict import StringPict
        >>> print(StringPict("T").subsuperindices(
        ...       StringPict("a"),StringPict("b,c")))
         b,c
        T
         a
        """
        width_self = self.width()
        width_index = max(sub_index.width(), super_index.width())
        extended_self = self.right(StringPict(width_index * " "))
        super_index = super_index.left(StringPict(width_self * " "))
        sub_index = sub_index.left(StringPict(width_self * " "))
        return extended_self.above(super_index, align="l").below(sub_index, align="l")

    def superindex(self, super_index):
        """
        Add a `super index`.

        Examples
        ========

        >>> from sympy.printing.pretty.stringpict import StringPict
        >>> print(StringPict("e").superindex(StringPict("-s(x)")))
         -s(x)
        e

        """
        width_self = self.width()
        width_index = super_index.width()
        extended_self = self.right(StringPict(width_index * " "))
        super_index = super_index.left(StringPict(width_self * " "))
        return extended_self.above(super_index)

    @staticmethod
    def stack(*args, align="c"):
        """Put pictures on top of each other,
        from top to bottom.
        Returns string, baseline arguments for StringPict.
        The baseline is the baseline of the second picture.
        By default, everything is centered. If `align` is set to
        'l' ('r') the elements are aligned to the  left (right) margin.
        Baseline is the baseline of the second picture.
        Strings are allowed.
        The special value StringPict.LINE is a
        row of '-' extended to the width.
        """
        # convert everything to StringPicts; keep LINE
        objects = []
        for arg in args:
            if arg is not StringPict.LINE and isinstance(arg, str):
                arg = StringPict(arg)
            objects.append(arg)

        # compute new width
        new_width = max(obj.width() for obj in objects if obj is not StringPict.LINE)

        line_obj = StringPict(hobj("-", new_width))

        # replace LINE with proper lines
        for i, obj in enumerate(objects):
            if obj is StringPict.LINE:
                objects[i] = line_obj

        # stack the pictures, and center the result
        if align == "c":
            new_picture = [
                center(line, new_width) for obj in objects for line in obj.picture
            ]
        elif align == "l":
            new_picture = [
                line + (new_width - len(line)) * " "
                for obj in objects
                for line in obj.picture
            ]
        elif align == "r":
            new_picture = [
                (new_width - len(line)) * " " + line
                for obj in objects
                for line in obj.picture
            ]
        else:
            raise ValueError(
                "the align parameter must be one of 'l'(left), "
                f"'r'(right) or 'c' (center). Got {align}."
            )

        new_baseline = objects[0].height() + objects[1].baseline
        return "\n".join(new_picture), new_baseline

    def terminal_width(self):
        """Return the terminal width if possible, otherwise return 0."""
        size = shutil.get_terminal_size(fallback=(0, 0))
        return size.columns

    def v_align(self, *args, align="t"):
        """
        Align a list of elements, keeping
        the baseline relative to self.
        """
        objects = []
        for arg in args:
            if isinstance(arg, str):
                arg = self.__class__(arg)
            objects.append(arg)

        baseline = self.baseline
        height = self.height()
        max_height = max((height, max(arg.height() for arg in objects)))
        texts = (str(obj) for obj in objects)

        if align == "b":
            if max_height > height:
                baseline = baseline + max_height - height

            offsets = (max_height - obj.height() for obj in objects)
            texts = (
                offset * "\n" + text if offset > 0 else text
                for offset, obj, text in zip(offsets, objects, texts)
            )
        elif align == "c":
            if max_height > height:
                baseline = baseline + (max_height - height + 1) // 2
            offsets = ((max_height - obj.height() + 1) // 2 for obj in objects)
            texts = (
                offset * "\n" + text if offset > 0 else text
                for offset, obj, text in zip(offsets, objects, texts)
            )
        elif align != "t":
            raise ValueError(f"'{align}' is not a valid value or the align parameter.")

        result = [self.__class__(text, baseline) for text in texts]
        return result

    def width(self):
        """The width of the picture in characters."""
        return line_width(self.picture[0])


class PrettyForm(StringPict):
    """
    Extension of the StringPict class that knows about basic math applications,
    optimizing double minus signs.

    "Binding" is interpreted as follows::

        ATOM this is an atom: never needs to be parenthesized
        FUNC this is a function application: parenthesize if added (?)
        DIV  this is a division: make wider division if divided
        POW  this is a power: only parenthesize if exponent
        MUL  this is a multiplication: parenthesize if powered
        ADD  this is an addition: parenthesize if multiplied or powered
        NEG  this is a negative number: optimize if added, parenthesize if
             multiplied or powered
        OPEN this is an open object: parenthesize if added, multiplied, or
             powered (example: Piecewise)
    """

    ATOM, FUNC, DIV, POW, MUL, ADD, NEG, OPEN = range(8)
    simpleFunctions = ["sin", "cos", "tan"]

    def __init__(self, text, baseline=0, binding=0, unicode=None):
        """Initialize from StringPict and binding power."""
        assert isinstance(text, str)
        assert isinstance(baseline, int)
        StringPict.__init__(self, text, baseline)
        self.binding = binding
        if unicode is not None:
            sympy_deprecation_warning(
                """
                The unicode argument to PrettyForm is deprecated. Only the text
                argument (the first positional argument) should be passed.
                """,
                deprecated_since_version="1.7",
                active_deprecations_target="deprecated-pretty-printing-functions",
            )
        self._unicode = unicode or self.text

    # Note: code to handle subtraction is in _print_Add

    def __add__(self, *others):
        """Make a pretty addition.
        Addition of negative numbers is simplified.
        """
        arg = self
        if arg.binding > PrettyForm.NEG:
            arg = arg.parenthesis()
        result = [arg]
        for arg in others:
            # add parentheses for weak binders
            if arg.binding > PrettyForm.NEG:
                arg = arg.parenthesis()
            # use existing minus sign if available
            if arg.binding != PrettyForm.NEG:
                result.append(" + ")
            result.append(arg)
        return PrettyForm(binding=PrettyForm.ADD, *StringPict.next(*result))

    def __mul__(self, *others):
        """Make a pretty multiplication.
        Parentheses are needed around +, - and neg.
        """
        quantity = {"degree": "\N{DEGREE SIGN}"}

        if len(others) == 0:
            # We aren't actually multiplying... So nothing to do here.
            return self

        # add parens on args that need them
        arg = self
        if arg.binding > PrettyForm.MUL and arg.binding != PrettyForm.NEG:
            arg = arg.parenthesis()
        result = [arg]
        for arg in others:
            if arg.picture[0] not in quantity.values():
                result.append(xsym("*"))
            # add parentheses for weak binders
            if arg.binding > PrettyForm.MUL and arg.binding != PrettyForm.NEG:
                arg = arg.parenthesis()
            result.append(arg)

        len_res = len(result)
        for i in range(len_res):
            if i < len_res - 1 and result[i] == "-1" and result[i + 1] == xsym("*"):
                # substitute -1 by -, like in -1*x -> -x
                result.pop(i)
                result.pop(i)
                result.insert(i, "-")
        if result[0][0] == "-":
            # if there is a - sign in front of all
            # This test was failing to catch a
            # PrettyForm.__mul__(PrettyForm("-1", 0, 6)) being negative
            binding = PrettyForm.NEG
            if result[0] == "-":
                right = result[1]
                if right.picture[right.baseline][0] == "-":
                    result[0] = "- "
        else:
            binding = PrettyForm.MUL
        return PrettyForm(binding=binding, *StringPict.next(*result))

    def __pow__(self, exponent):
        """Make a pretty power."""
        base = self
        use_inline_func_form = False
        if exponent.binding == PrettyForm.POW:
            exponent = exponent.parenthesis()
        if base.binding > PrettyForm.FUNC:
            base = base.parenthesis()
        elif base.binding == PrettyForm.FUNC:
            # heuristic for when to use inline power
            if exponent.height() > 1:
                base = base.parenthesis()
            else:
                use_inline_func_form = True

        if use_inline_func_form:
            #         2
            #  sin  +   + (x)
            # b.baseline = a.prettyFunc.baseline + b.height()
            # func = a.prettyFunc.right(b)
            # return func.right(a.prettyArgs)
            func = base.prettyFunc.superindex(exponent)
            return func.right(base.prettyArgs)

        #      2    <-- top
        # (x+y)     <-- bot
        # top = b.left(' ' * a.width())
        # bot = a.right(' ' * b.width())

        # result = bot.above(top)
        result = base.superindex(exponent)
        result.binding = PrettyForm.POW
        return result

    def __repr__(self):
        return "PrettyForm(%r,%d,%d)" % (
            "\n".join(self.picture),
            self.baseline,
            self.binding,
        )

    def __truediv__(self, den, slashed=False):
        """Make a pretty division; stacked or slashed."""
        if slashed:
            raise NotImplementedError("Can't do slashed fraction yet")
        num = self
        if num.binding == PrettyForm.DIV:
            num = num.parenthesis()
        if den.binding == PrettyForm.DIV:
            den = den.parenthesis()

        if num.binding == PrettyForm.NEG:
            num = num.right(" ")

        return PrettyForm(
            binding=PrettyForm.DIV, *StringPict.stack(num, StringPict.LINE, den)
        )

    @staticmethod
    def apply(function, *args):
        """Functions of one or more variables."""
        if function in PrettyForm.simpleFunctions:
            # simple function: use only space if possible
            assert len(args) == 1, f"Simple function {function} must have 1 argument"
            arg = args[0].__pretty__()
            if arg.binding <= PrettyForm.DIV:
                # optimization: no parentheses necessary
                result = arg.left(function + " ")
                result.binding = PrettyForm.FUNC
                return result
        argument_list: list = []
        for arg in args:
            argument_list.append(",")
            argument_list.append(arg.__pretty__())
        if argument_list:
            first_arg, *rest = argument_list
            argument_pict = first_arg.right(*rest)
        else:
            argument_pict = StringPict("")
        argument_pict = argument_pict.parenthesis()
        func_pict = argument_pict.left(function)
        func_pict.binding = PrettyForm.ATOM
        return func_pict

    @property
    def unicode(self):
        """
        Unicode version of the `text` attribute.
        (Deprecated since v1.7)
        """
        sympy_deprecation_warning(
            """
            The PrettyForm.unicode attribute is deprecated. Use the
            PrettyForm.text attribute instead.
            """,
            deprecated_since_version="1.7",
            active_deprecations_target="deprecated-pretty-printing-functions",
        )
        return self._unicode


# backward compatibility
stringPict = StringPict
prettyForm = PrettyForm
