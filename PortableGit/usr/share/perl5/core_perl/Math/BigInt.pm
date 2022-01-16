# -*- coding: utf-8-unix -*-

package Math::BigInt;

#
# "Mike had an infinite amount to do and a negative amount of time in which
# to do it." - Before and After
#

# The following hash values are used:
#   value: unsigned int with actual value (as a Math::BigInt::Calc or similar)
#   sign : +, -, NaN, +inf, -inf
#   _a   : accuracy
#   _p   : precision

# Remember not to take shortcuts ala $xs = $x->{value}; $LIB->foo($xs); since
# underlying lib might change the reference!

use 5.006001;
use strict;
use warnings;

use Carp qw< carp croak >;

our $VERSION = '1.999818';

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(objectify bgcd blcm);

# Inside overload, the first arg is always an object. If the original code had
# it reversed (like $x = 2 * $y), then the third parameter is true.
# In some cases (like add, $x = $x + 2 is the same as $x = 2 + $x) this makes
# no difference, but in some cases it does.

# For overloaded ops with only one argument we simple use $_[0]->copy() to
# preserve the argument.

# Thus inheritance of overload operators becomes possible and transparent for
# our subclasses without the need to repeat the entire overload section there.

use overload

  # overload key: with_assign

  '+'     =>      sub { $_[0] -> copy() -> badd($_[1]); },

  '-'     =>      sub { my $c = $_[0] -> copy;
                        $_[2] ? $c -> bneg() -> badd($_[1])
                              : $c -> bsub($_[1]); },

  '*'     =>      sub { $_[0] -> copy() -> bmul($_[1]); },

  '/'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bdiv($_[0])
                              : $_[0] -> copy -> bdiv($_[1]); },

  '%'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bmod($_[0])
                              : $_[0] -> copy -> bmod($_[1]); },

  '**'    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bpow($_[0])
                              : $_[0] -> copy -> bpow($_[1]); },

  '<<'    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> blsft($_[0])
                              : $_[0] -> copy -> blsft($_[1]); },

  '>>'    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> brsft($_[0])
                              : $_[0] -> copy -> brsft($_[1]); },

  # overload key: assign

  '+='    =>      sub { $_[0]->badd($_[1]); },

  '-='    =>      sub { $_[0]->bsub($_[1]); },

  '*='    =>      sub { $_[0]->bmul($_[1]); },

  '/='    =>      sub { scalar $_[0]->bdiv($_[1]); },

  '%='    =>      sub { $_[0]->bmod($_[1]); },

  '**='   =>      sub { $_[0]->bpow($_[1]); },

  '<<='   =>      sub { $_[0]->blsft($_[1]); },

  '>>='   =>      sub { $_[0]->brsft($_[1]); },

#  'x='    =>      sub { },

#  '.='    =>      sub { },

  # overload key: num_comparison

  '<'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> blt($_[0])
                              : $_[0] -> blt($_[1]); },

  '<='    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> ble($_[0])
                              : $_[0] -> ble($_[1]); },

  '>'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bgt($_[0])
                              : $_[0] -> bgt($_[1]); },

  '>='    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bge($_[0])
                              : $_[0] -> bge($_[1]); },

  '=='    =>      sub { $_[0] -> beq($_[1]); },

  '!='    =>      sub { $_[0] -> bne($_[1]); },

  # overload key: 3way_comparison

  '<=>'   =>      sub { my $cmp = $_[0] -> bcmp($_[1]);
                        defined($cmp) && $_[2] ? -$cmp : $cmp; },

  'cmp'   =>      sub { $_[2] ? "$_[1]" cmp $_[0] -> bstr()
                              : $_[0] -> bstr() cmp "$_[1]"; },

  # overload key: str_comparison

#  'lt'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bstrlt($_[0])
#                              : $_[0] -> bstrlt($_[1]); },
#
#  'le'    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bstrle($_[0])
#                              : $_[0] -> bstrle($_[1]); },
#
#  'gt'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bstrgt($_[0])
#                              : $_[0] -> bstrgt($_[1]); },
#
#  'ge'    =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bstrge($_[0])
#                              : $_[0] -> bstrge($_[1]); },
#
#  'eq'    =>      sub { $_[0] -> bstreq($_[1]); },
#
#  'ne'    =>      sub { $_[0] -> bstrne($_[1]); },

  # overload key: binary

  '&'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> band($_[0])
                              : $_[0] -> copy -> band($_[1]); },

  '&='    =>      sub { $_[0] -> band($_[1]); },

  '|'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bior($_[0])
                              : $_[0] -> copy -> bior($_[1]); },

  '|='    =>      sub { $_[0] -> bior($_[1]); },

  '^'     =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> bxor($_[0])
                              : $_[0] -> copy -> bxor($_[1]); },

  '^='    =>      sub { $_[0] -> bxor($_[1]); },

#  '&.'    =>      sub { },

#  '&.='   =>      sub { },

#  '|.'    =>      sub { },

#  '|.='   =>      sub { },

#  '^.'    =>      sub { },

#  '^.='   =>      sub { },

  # overload key: unary

  'neg'   =>      sub { $_[0] -> copy() -> bneg(); },

#  '!'     =>      sub { },

  '~'     =>      sub { $_[0] -> copy() -> bnot(); },

#  '~.'    =>      sub { },

  # overload key: mutators

  '++'    =>      sub { $_[0] -> binc() },

  '--'    =>      sub { $_[0] -> bdec() },

  # overload key: func

  'atan2' =>      sub { $_[2] ? ref($_[0]) -> new($_[1]) -> batan2($_[0])
                              : $_[0] -> copy() -> batan2($_[1]); },

  'cos'   =>      sub { $_[0] -> copy -> bcos(); },

  'sin'   =>      sub { $_[0] -> copy -> bsin(); },

  'exp'   =>      sub { $_[0] -> copy() -> bexp($_[1]); },

  'abs'   =>      sub { $_[0] -> copy() -> babs(); },

  'log'   =>      sub { $_[0] -> copy() -> blog(); },

  'sqrt'  =>      sub { $_[0] -> copy() -> bsqrt(); },

  'int'   =>      sub { $_[0] -> copy() -> bint(); },

  # overload key: conversion

  'bool'  =>      sub { $_[0] -> is_zero() ? '' : 1; },

  '""'    =>      sub { $_[0] -> bstr(); },

  '0+'    =>      sub { $_[0] -> numify(); },

  '='     =>      sub { $_[0]->copy(); },

  ;

##############################################################################
# global constants, flags and accessory

# These vars are public, but their direct usage is not recommended, use the
# accessor methods instead

our $round_mode = 'even'; # one of 'even', 'odd', '+inf', '-inf', 'zero', 'trunc' or 'common'
our $accuracy   = undef;
our $precision  = undef;
our $div_scale  = 40;
our $upgrade    = undef;                    # default is no upgrade
our $downgrade  = undef;                    # default is no downgrade

# These are internally, and not to be used from the outside at all

our $_trap_nan = 0;                         # are NaNs ok? set w/ config()
our $_trap_inf = 0;                         # are infs ok? set w/ config()

my $nan = 'NaN';                        # constants for easier life

my $LIB = 'Math::BigInt::Calc';        # module to do the low level math
                                        # default is Calc.pm
my $IMPORT = 0;                         # was import() called yet?
                                        # used to make require work
my %CALLBACKS;                          # callbacks to notify on lib loads

##############################################################################
# the old code had $rnd_mode, so we need to support it, too

our $rnd_mode   = 'even';

sub TIESCALAR {
    my ($class) = @_;
    bless \$round_mode, $class;
}

sub FETCH {
    return $round_mode;
}

sub STORE {
    $rnd_mode = $_[0]->round_mode($_[1]);
}

BEGIN {
    # tie to enable $rnd_mode to work transparently
    tie $rnd_mode, 'Math::BigInt';

    # set up some handy alias names
    *as_int = \&as_number;
    *is_pos = \&is_positive;
    *is_neg = \&is_negative;
}

###############################################################################
# Configuration methods
###############################################################################

sub round_mode {
    no strict 'refs';
    # make Class->round_mode() work
    my $self = shift;
    my $class = ref($self) || $self || __PACKAGE__;
    if (defined $_[0]) {
        my $m = shift;
        if ($m !~ /^(even|odd|\+inf|\-inf|zero|trunc|common)$/) {
            croak("Unknown round mode '$m'");
        }
        return ${"${class}::round_mode"} = $m;
    }
    ${"${class}::round_mode"};
}

sub upgrade {
    no strict 'refs';
    # make Class->upgrade() work
    my $self = shift;
    my $class = ref($self) || $self || __PACKAGE__;
    # need to set new value?
    if (@_ > 0) {
        return ${"${class}::upgrade"} = $_[0];
    }
    ${"${class}::upgrade"};
}

sub downgrade {
    no strict 'refs';
    # make Class->downgrade() work
    my $self = shift;
    my $class = ref($self) || $self || __PACKAGE__;
    # need to set new value?
    if (@_ > 0) {
        return ${"${class}::downgrade"} = $_[0];
    }
    ${"${class}::downgrade"};
}

sub div_scale {
    no strict 'refs';
    # make Class->div_scale() work
    my $self = shift;
    my $class = ref($self) || $self || __PACKAGE__;
    if (defined $_[0]) {
        if ($_[0] < 0) {
            croak('div_scale must be greater than zero');
        }
        ${"${class}::div_scale"} = $_[0];
    }
    ${"${class}::div_scale"};
}

sub accuracy {
    # $x->accuracy($a);           ref($x) $a
    # $x->accuracy();             ref($x)
    # Class->accuracy();          class
    # Class->accuracy($a);        class $a

    my $x = shift;
    my $class = ref($x) || $x || __PACKAGE__;

    no strict 'refs';
    if (@_ > 0) {
        my $a = shift;
        if (defined $a) {
            $a = $a->numify() if ref($a) && $a->can('numify');
            # also croak on non-numerical
            if (!$a || $a <= 0) {
                croak('Argument to accuracy must be greater than zero');
            }
            if (int($a) != $a) {
                croak('Argument to accuracy must be an integer');
            }
        }

        if (ref($x)) {
            # Set instance variable.
            $x->bround($a) if $a; # not for undef, 0
            $x->{_a} = $a;        # set/overwrite, even if not rounded
            delete $x->{_p};      # clear P
            # Why return class variable here? Fixme!
            $a = ${"${class}::accuracy"} unless defined $a; # proper return value
        } else {
            # Set class variable.
            ${"${class}::accuracy"} = $a; # set global A
            ${"${class}::precision"} = undef; # clear global P
        }

        return $a;              # shortcut
    }

    # Return instance variable.
    return $x->{_a} if ref($x) && (defined $x->{_a} || defined $x->{_p});

    # Return class variable.
    return ${"${class}::accuracy"};
}

sub precision {
    # $x->precision($p);          ref($x) $p
    # $x->precision();            ref($x)
    # Class->precision();         class
    # Class->precision($p);       class $p

    my $x = shift;
    my $class = ref($x) || $x || __PACKAGE__;

    no strict 'refs';
    if (@_ > 0) {
        my $p = shift;
        if (defined $p) {
            $p = $p->numify() if ref($p) && $p->can('numify');
            if ($p != int $p) {
                croak('Argument to precision must be an integer');
            }
        }

        if (ref($x)) {
            # Set instance variable.
            $x->bfround($p) if $p; # not for undef, 0
            $x->{_p} = $p;         # set/overwrite, even if not rounded
            delete $x->{_a};       # clear A
            # Why return class variable here? Fixme!
            $p = ${"${class}::precision"} unless defined $p; # proper return value
        } else {
            # Set class variable.
            ${"${class}::precision"} = $p; # set global P
            ${"${class}::accuracy"} = undef; # clear global A
        }

        return $p;              # shortcut
    }

    # Return instance variable.
    return $x->{_p} if ref($x) && (defined $x->{_a} || defined $x->{_p});

    # Return class variable.
    return ${"${class}::precision"};
}

sub config {
    # return (or set) configuration data.
    my $class = shift || __PACKAGE__;

    no strict 'refs';
    if (@_ > 1 || (@_ == 1 && (ref($_[0]) eq 'HASH'))) {
        # try to set given options as arguments from hash

        my $args = $_[0];
        if (ref($args) ne 'HASH') {
            $args = { @_ };
        }
        # these values can be "set"
        my $set_args = {};
        foreach my $key (qw/
                               accuracy precision
                               round_mode div_scale
                               upgrade downgrade
                               trap_inf trap_nan
                           /)
        {
            $set_args->{$key} = $args->{$key} if exists $args->{$key};
            delete $args->{$key};
        }
        if (keys %$args > 0) {
            croak("Illegal key(s) '", join("', '", keys %$args),
                        "' passed to $class\->config()");
        }
        foreach my $key (keys %$set_args) {
            if ($key =~ /^trap_(inf|nan)\z/) {
                ${"${class}::_trap_$1"} = ($set_args->{"trap_$1"} ? 1 : 0);
                next;
            }
            # use a call instead of just setting the $variable to check argument
            $class->$key($set_args->{$key});
        }
    }

    # now return actual configuration

    my $cfg = {
               lib         => $LIB,
               lib_version => ${"${LIB}::VERSION"},
               class       => $class,
               trap_nan    => ${"${class}::_trap_nan"},
               trap_inf    => ${"${class}::_trap_inf"},
               version     => ${"${class}::VERSION"},
              };
    foreach my $key (qw/
                           accuracy precision
                           round_mode div_scale
                           upgrade downgrade
                       /)
    {
        $cfg->{$key} = ${"${class}::$key"};
    }
    if (@_ == 1 && (ref($_[0]) ne 'HASH')) {
        # calls of the style config('lib') return just this value
        return $cfg->{$_[0]};
    }
    $cfg;
}

sub _scale_a {
    # select accuracy parameter based on precedence,
    # used by bround() and bfround(), may return undef for scale (means no op)
    my ($x, $scale, $mode) = @_;

    $scale = $x->{_a} unless defined $scale;

    no strict 'refs';
    my $class = ref($x);

    $scale = ${ $class . '::accuracy' } unless defined $scale;
    $mode = ${ $class . '::round_mode' } unless defined $mode;

    if (defined $scale) {
        $scale = $scale->can('numify') ? $scale->numify()
                                       : "$scale" if ref($scale);
        $scale = int($scale);
    }

    ($scale, $mode);
}

sub _scale_p {
    # select precision parameter based on precedence,
    # used by bround() and bfround(), may return undef for scale (means no op)
    my ($x, $scale, $mode) = @_;

    $scale = $x->{_p} unless defined $scale;

    no strict 'refs';
    my $class = ref($x);

    $scale = ${ $class . '::precision' } unless defined $scale;
    $mode = ${ $class . '::round_mode' } unless defined $mode;

    if (defined $scale) {
        $scale = $scale->can('numify') ? $scale->numify()
                                       : "$scale" if ref($scale);
        $scale = int($scale);
    }

    ($scale, $mode);
}

###############################################################################
# Constructor methods
###############################################################################

sub new {
    # Create a new Math::BigInt object from a string or another Math::BigInt
    # object. See hash keys documented at top.

    # The argument could be an object, so avoid ||, && etc. on it. This would
    # cause costly overloaded code to be called. The only allowed ops are ref()
    # and defined.

    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # The POD says:
    #
    # "Currently, Math::BigInt->new() defaults to 0, while Math::BigInt->new('')
    # results in 'NaN'. This might change in the future, so use always the
    # following explicit forms to get a zero or NaN:
    #     $zero = Math::BigInt->bzero();
    #     $nan = Math::BigInt->bnan();
    #
    # But although this use has been discouraged for more than 10 years, people
    # apparently still use it, so we still support it.

    return $self->bzero() unless @_;

    my ($wanted, $a, $p, $r) = @_;

    # Always return a new object, so if called as an instance method, copy the
    # invocand, and if called as a class method, initialize a new object.

    $self = $selfref ? $self -> copy()
                     : bless {}, $class;

    unless (defined $wanted) {
        #carp("Use of uninitialized value in new()");
        return $self->bzero($a, $p, $r);
    }

    if (ref($wanted) && $wanted->isa($class)) {         # MBI or subclass
        # Using "$copy = $wanted -> copy()" here fails some tests. Fixme!
        my $copy = $class -> copy($wanted);
        if ($selfref) {
            %$self = %$copy;
        } else {
            $self = $copy;
        }
        return $self;
    }

    $class->import() if $IMPORT == 0;           # make require work

    # Shortcut for non-zero scalar integers with no non-zero exponent.

    if (!ref($wanted) &&
        $wanted =~ / ^
                     ([+-]?)            # optional sign
                     ([1-9][0-9]*)      # non-zero significand
                     (\.0*)?            # ... with optional zero fraction
                     ([Ee][+-]?0+)?     # optional zero exponent
                     \z
                   /x)
    {
        my $sgn = $1;
        my $abs = $2;
        $self->{sign} = $sgn || '+';
        $self->{value} = $LIB->_new($abs);

        no strict 'refs';
        if (defined($a) || defined($p)
            || defined(${"${class}::precision"})
            || defined(${"${class}::accuracy"}))
        {
            $self->round($a, $p, $r)
              unless @_ >= 3 && !defined $a && !defined $p;
        }

        return $self;
    }

    # Handle Infs.

    if ($wanted =~ /^\s*([+-]?)inf(inity)?\s*\z/i) {
        my $sgn = $1 || '+';
        $self->{sign} = $sgn . 'inf';   # set a default sign for bstr()
        return $class->binf($sgn);
    }

    # Handle explicit NaNs (not the ones returned due to invalid input).

    if ($wanted =~ /^\s*([+-]?)nan\s*\z/i) {
        $self = $class -> bnan();
        $self->round($a, $p, $r) unless @_ >= 3 && !defined $a && !defined $p;
        return $self;
    }

    # Handle hexadecimal numbers.

    if ($wanted =~ /^\s*[+-]?0[Xx]/) {
        $self = $class -> from_hex($wanted);
        $self->round($a, $p, $r) unless @_ >= 3 && !defined $a && !defined $p;
        return $self;
    }

    # Handle binary numbers.

    if ($wanted =~ /^\s*[+-]?0[Bb]/) {
        $self = $class -> from_bin($wanted);
        $self->round($a, $p, $r) unless @_ >= 3 && !defined $a && !defined $p;
        return $self;
    }

    # Split string into mantissa, exponent, integer, fraction, value, and sign.
    my ($mis, $miv, $mfv, $es, $ev) = _split($wanted);
    if (!ref $mis) {
        if ($_trap_nan) {
            croak("$wanted is not a number in $class");
        }
        $self->{value} = $LIB->_zero();
        $self->{sign} = $nan;
        return $self;
    }

    if (!ref $miv) {
        # _from_hex or _from_bin
        $self->{value} = $mis->{value};
        $self->{sign} = $mis->{sign};
        return $self;   # throw away $mis
    }

    # Make integer from mantissa by adjusting exponent, then convert to a
    # Math::BigInt.
    $self->{sign} = $$mis;           # store sign
    $self->{value} = $LIB->_zero(); # for all the NaN cases
    my $e = int("$$es$$ev");         # exponent (avoid recursion)
    if ($e > 0) {
        my $diff = $e - CORE::length($$mfv);
        if ($diff < 0) {         # Not integer
            if ($_trap_nan) {
                croak("$wanted not an integer in $class");
            }
            #print "NOI 1\n";
            return $upgrade->new($wanted, $a, $p, $r) if defined $upgrade;
            $self->{sign} = $nan;
        } else {                 # diff >= 0
            # adjust fraction and add it to value
            #print "diff > 0 $$miv\n";
            $$miv = $$miv . ($$mfv . '0' x $diff);
        }
    }

    else {
        if ($$mfv ne '') {       # e <= 0
            # fraction and negative/zero E => NOI
            if ($_trap_nan) {
                croak("$wanted not an integer in $class");
            }
            #print "NOI 2 \$\$mfv '$$mfv'\n";
            return $upgrade->new($wanted, $a, $p, $r) if defined $upgrade;
            $self->{sign} = $nan;
        } elsif ($e < 0) {
            # xE-y, and empty mfv
            # Split the mantissa at the decimal point. E.g., if
            # $$miv = 12345 and $e = -2, then $frac = 45 and $$miv = 123.

            my $frac = substr($$miv, $e); # $frac is fraction part
            substr($$miv, $e) = "";       # $$miv is now integer part

            if ($frac =~ /[^0]/) {
                if ($_trap_nan) {
                    croak("$wanted not an integer in $class");
                }
                #print "NOI 3\n";
                return $upgrade->new($wanted, $a, $p, $r) if defined $upgrade;
                $self->{sign} = $nan;
            }
        }
    }

    unless ($self->{sign} eq $nan) {
        $self->{sign} = '+' if $$miv eq '0';            # normalize -0 => +0
        $self->{value} = $LIB->_new($$miv) if $self->{sign} =~ /^[+-]$/;
    }

    # If any of the globals are set, use them to round, and store them inside
    # $self. Do not round for new($x, undef, undef) since that is used by MBF
    # to signal no rounding.

    $self->round($a, $p, $r) unless @_ >= 3 && !defined $a && !defined $p;
    $self;
}

# Create a Math::BigInt from a hexadecimal string.

sub from_hex {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Don't modify constant (read-only) objects.

    return if $selfref && $self->modify('from_hex');

    my $str = shift;

    # If called as a class method, initialize a new object.

    $self = $class -> bzero() unless $selfref;

    if ($str =~ s/
                     ^
                     \s*
                     ( [+-]? )
                     (0?x)?
                     (
                         [0-9a-fA-F]*
                         ( _ [0-9a-fA-F]+ )*
                     )
                     \s*
                     $
                 //x)
    {
        # Get a "clean" version of the string, i.e., non-emtpy and with no
        # underscores or invalid characters.

        my $sign = $1;
        my $chrs = $3;
        $chrs =~ tr/_//d;
        $chrs = '0' unless CORE::length $chrs;

        # The library method requires a prefix.

        $self->{value} = $LIB->_from_hex('0x' . $chrs);

        # Place the sign.

        $self->{sign} = $sign eq '-' && ! $LIB->_is_zero($self->{value})
                          ? '-' : '+';

        return $self;
    }

    # CORE::hex() parses as much as it can, and ignores any trailing garbage.
    # For backwards compatibility, we return NaN.

    return $self->bnan();
}

# Create a Math::BigInt from an octal string.

sub from_oct {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Don't modify constant (read-only) objects.

    return if $selfref && $self->modify('from_oct');

    my $str = shift;

    # If called as a class method, initialize a new object.

    $self = $class -> bzero() unless $selfref;

    if ($str =~ s/
                     ^
                     \s*
                     ( [+-]? )
                     (
                         [0-7]*
                         ( _ [0-7]+ )*
                     )
                     \s*
                     $
                 //x)
    {
        # Get a "clean" version of the string, i.e., non-emtpy and with no
        # underscores or invalid characters.

        my $sign = $1;
        my $chrs = $2;
        $chrs =~ tr/_//d;
        $chrs = '0' unless CORE::length $chrs;

        # The library method requires a prefix.

        $self->{value} = $LIB->_from_oct('0' . $chrs);

        # Place the sign.

        $self->{sign} = $sign eq '-' && ! $LIB->_is_zero($self->{value})
                          ? '-' : '+';

        return $self;
    }

    # CORE::oct() parses as much as it can, and ignores any trailing garbage.
    # For backwards compatibility, we return NaN.

    return $self->bnan();
}

# Create a Math::BigInt from a binary string.

sub from_bin {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Don't modify constant (read-only) objects.

    return if $selfref && $self->modify('from_bin');

    my $str = shift;

    # If called as a class method, initialize a new object.

    $self = $class -> bzero() unless $selfref;

    if ($str =~ s/
                     ^
                     \s*
                     ( [+-]? )
                     (0?b)?
                     (
                         [01]*
                         ( _ [01]+ )*
                     )
                     \s*
                     $
                 //x)
    {
        # Get a "clean" version of the string, i.e., non-emtpy and with no
        # underscores or invalid characters.

        my $sign = $1;
        my $chrs = $3;
        $chrs =~ tr/_//d;
        $chrs = '0' unless CORE::length $chrs;

        # The library method requires a prefix.

        $self->{value} = $LIB->_from_bin('0b' . $chrs);

        # Place the sign.

        $self->{sign} = $sign eq '-' && ! $LIB->_is_zero($self->{value})
                          ? '-' : '+';

        return $self;
    }

    # For consistency with from_hex() and from_oct(), we return NaN when the
    # input is invalid.

    return $self->bnan();

}

# Create a Math::BigInt from a byte string.

sub from_bytes {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Don't modify constant (read-only) objects.

    return if $selfref && $self->modify('from_bytes');

    croak("from_bytes() requires a newer version of the $LIB library.")
        unless $LIB->can('_from_bytes');

    my $str = shift;

    # If called as a class method, initialize a new object.

    $self = $class -> bzero() unless $selfref;
    $self -> {sign}  = '+';
    $self -> {value} = $LIB -> _from_bytes($str);
    return $self;
}

sub from_base {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # Don't modify constant (read-only) objects.

    return if $selfref && $self->modify('from_base');

    my $str = shift;

    my $base = shift;
    $base = $class->new($base) unless ref($base);

    croak("the base must be a finite integer >= 2")
      if $base < 2 || ! $base -> is_int();

    # If called as a class method, initialize a new object.

    $self = $class -> bzero() unless $selfref;

    # If no collating sequence is given, pass some of the conversions to
    # methods optimized for those cases.

    if (! @_) {
        return $self -> from_bin($str) if $base == 2;
        return $self -> from_oct($str) if $base == 8;
        return $self -> from_hex($str) if $base == 16;
        if ($base == 10) {
            my $tmp = $class -> new($str);
            $self -> {value} = $tmp -> {value};
            $self -> {sign}  = '+';
        }
    }

    croak("from_base() requires a newer version of the $LIB library.")
      unless $LIB->can('_from_base');

    $self -> {sign}  = '+';
    $self -> {value}
      = $LIB->_from_base($str, $base -> {value}, @_ ? shift() : ());
    return $self
}

sub bzero {
    # create/assign '+0'

    if (@_ == 0) {
        #carp("Using bzero() as a function is deprecated;",
        #           " use bzero() as a method instead");
        unshift @_, __PACKAGE__;
    }

    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    $self->import() if $IMPORT == 0;            # make require work

    # Don't modify constant (read-only) objects.

    return if $selfref && $self->modify('bzero');

    $self = bless {}, $class unless $selfref;

    $self->{sign} = '+';
    $self->{value} = $LIB->_zero();

    # If rounding parameters are given as arguments, use them. If no rounding
    # parameters are given, and if called as a class method initialize the new
    # instance with the class variables.

    if (@_) {
        croak "can't specify both accuracy and precision"
          if @_ >= 2 && defined $_[0] && defined $_[1];
        $self->{_a} = $_[0];
        $self->{_p} = $_[1];
    } else {
        unless($selfref) {
            $self->{_a} = $class -> accuracy();
            $self->{_p} = $class -> precision();
        }
    }

    return $self;
}

sub bone {
    # Create or assign '+1' (or -1 if given sign '-').

    if (@_ == 0 || (defined($_[0]) && ($_[0] eq '+' || $_[0] eq '-'))) {
        #carp("Using bone() as a function is deprecated;",
        #           " use bone() as a method instead");
        unshift @_, __PACKAGE__;
    }

    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    $self->import() if $IMPORT == 0;            # make require work

    # Don't modify constant (read-only) objects.

    return if $selfref && $self->modify('bone');

    my $sign = '+';             # default
    if (@_) {
        $sign = shift;
        $sign = $sign =~ /^\s*-/ ? "-" : "+";
    }

    $self = bless {}, $class unless $selfref;

    $self->{sign}  = $sign;
    $self->{value} = $LIB->_one();

    # If rounding parameters are given as arguments, use them. If no rounding
    # parameters are given, and if called as a class method initialize the new
    # instance with the class variables.

    if (@_) {
        croak "can't specify both accuracy and precision"
          if @_ >= 2 && defined $_[0] && defined $_[1];
        $self->{_a} = $_[0];
        $self->{_p} = $_[1];
    } else {
        unless($selfref) {
            $self->{_a} = $class -> accuracy();
            $self->{_p} = $class -> precision();
        }
    }

    return $self;
}

sub binf {
    # create/assign a '+inf' or '-inf'

    if (@_ == 0 || (defined($_[0]) && !ref($_[0]) &&
                    $_[0] =~ /^\s*[+-](inf(inity)?)?\s*$/))
    {
        #carp("Using binf() as a function is deprecated;",
        #           " use binf() as a method instead");
        unshift @_, __PACKAGE__;
    }

    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    {
        no strict 'refs';
        if (${"${class}::_trap_inf"}) {
            croak("Tried to create +-inf in $class->binf()");
        }
    }

    $self->import() if $IMPORT == 0;            # make require work

    # Don't modify constant (read-only) objects.

    return if $selfref && $self->modify('binf');

    my $sign = shift;
    $sign = defined $sign && $sign =~ /^\s*-/ ? "-" : "+";

    $self = bless {}, $class unless $selfref;

    $self -> {sign}  = $sign . 'inf';
    $self -> {value} = $LIB -> _zero();

    # If rounding parameters are given as arguments, use them. If no rounding
    # parameters are given, and if called as a class method initialize the new
    # instance with the class variables.

    if (@_) {
        croak "can't specify both accuracy and precision"
          if @_ >= 2 && defined $_[0] && defined $_[1];
        $self->{_a} = $_[0];
        $self->{_p} = $_[1];
    } else {
        unless($selfref) {
            $self->{_a} = $class -> accuracy();
            $self->{_p} = $class -> precision();
        }
    }

    return $self;
}

sub bnan {
    # create/assign a 'NaN'

    if (@_ == 0) {
        #carp("Using bnan() as a function is deprecated;",
        #           " use bnan() as a method instead");
        unshift @_, __PACKAGE__;
    }

    my $self    = shift;
    my $selfref = ref($self);
    my $class   = $selfref || $self;

    {
        no strict 'refs';
        if (${"${class}::_trap_nan"}) {
            croak("Tried to create NaN in $class->bnan()");
        }
    }

    $self->import() if $IMPORT == 0;            # make require work

    # Don't modify constant (read-only) objects.

    return if $selfref && $self->modify('bnan');

    $self = bless {}, $class unless $selfref;

    $self -> {sign}  = $nan;
    $self -> {value} = $LIB -> _zero();

    return $self;
}

sub bpi {
    # Calculate PI to N digits. Unless upgrading is in effect, returns the
    # result truncated to an integer, that is, always returns '3'.
    my ($self, $n) = @_;
    if (@_ == 1) {
        # called like Math::BigInt::bpi(10);
        $n = $self;
        $self = __PACKAGE__;
    }
    $self = ref($self) if ref($self);

    return $upgrade->new($n) if defined $upgrade;

    # hard-wired to "3"
    $self->new(3);
}

sub copy {
    my $self    = shift;
    my $selfref = ref $self;
    my $class   = $selfref || $self;

    # If called as a class method, the object to copy is the next argument.

    $self = shift() unless $selfref;

    my $copy = bless {}, $class;

    $copy->{sign}  = $self->{sign};
    $copy->{value} = $LIB->_copy($self->{value});
    $copy->{_a}    = $self->{_a} if exists $self->{_a};
    $copy->{_p}    = $self->{_p} if exists $self->{_p};

    return $copy;
}

sub as_number {
    # An object might be asked to return itself as bigint on certain overloaded
    # operations. This does exactly this, so that sub classes can simple inherit
    # it or override with their own integer conversion routine.
    $_[0]->copy();
}

###############################################################################
# Boolean methods
###############################################################################

sub is_zero {
    # return true if arg (BINT or num_str) is zero (array '+', '0')
    my ($class, $x) = ref($_[0]) ? (undef, $_[0]) : objectify(1, @_);

    return 0 if $x->{sign} !~ /^\+$/; # -, NaN & +-inf aren't
    $LIB->_is_zero($x->{value});
}

sub is_one {
    # return true if arg (BINT or num_str) is +1, or -1 if sign is given
    my ($class, $x, $sign) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    $sign = '+' if !defined $sign || $sign ne '-';

    return 0 if $x->{sign} ne $sign; # -1 != +1, NaN, +-inf aren't either
    $LIB->_is_one($x->{value});
}

sub is_finite {
    my $x = shift;
    return $x->{sign} eq '+' || $x->{sign} eq '-';
}

sub is_inf {
    # return true if arg (BINT or num_str) is +-inf
    my ($class, $x, $sign) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    if (defined $sign) {
        $sign = '[+-]inf' if $sign eq ''; # +- doesn't matter, only that's inf
        $sign = "[$1]inf" if $sign =~ /^([+-])(inf)?$/; # extract '+' or '-'
        return $x->{sign} =~ /^$sign$/ ? 1 : 0;
    }
    $x->{sign} =~ /^[+-]inf$/ ? 1 : 0; # only +-inf is infinity
}

sub is_nan {
    # return true if arg (BINT or num_str) is NaN
    my ($class, $x) = ref($_[0]) ? (undef, $_[0]) : objectify(1, @_);

    $x->{sign} eq $nan ? 1 : 0;
}

sub is_positive {
    # return true when arg (BINT or num_str) is positive (> 0)
    my ($class, $x) = ref($_[0]) ? (undef, $_[0]) : objectify(1, @_);

    return 1 if $x->{sign} eq '+inf'; # +inf is positive

    # 0+ is neither positive nor negative
    ($x->{sign} eq '+' && !$x->is_zero()) ? 1 : 0;
}

sub is_negative {
    # return true when arg (BINT or num_str) is negative (< 0)
    my ($class, $x) = ref($_[0]) ? (undef, $_[0]) : objectify(1, @_);

    $x->{sign} =~ /^-/ ? 1 : 0; # -inf is negative, but NaN is not
}

sub is_non_negative {
    # Return true if argument is non-negative (>= 0).
    my ($class, $x) = ref($_[0]) ? (undef,$_[0]) : objectify(1,@_);

    return 1 if $x->{sign} =~ /^\+/;
    return 1 if $x -> is_zero();
    return 0;
}

sub is_non_positive {
    # Return true if argument is non-positive (<= 0).
    my ($class, $x) = ref($_[0]) ? (undef,$_[0]) : objectify(1,@_);

    return 1 if $x->{sign} =~ /^\-/;
    return 1 if $x -> is_zero();
    return 0;
}

sub is_odd {
    # return true when arg (BINT or num_str) is odd, false for even
    my ($class, $x) = ref($_[0]) ? (undef, $_[0]) : objectify(1, @_);

    return 0 if $x->{sign} !~ /^[+-]$/; # NaN & +-inf aren't
    $LIB->_is_odd($x->{value});
}

sub is_even {
    # return true when arg (BINT or num_str) is even, false for odd
    my ($class, $x) = ref($_[0]) ? (undef, $_[0]) : objectify(1, @_);

    return 0 if $x->{sign} !~ /^[+-]$/; # NaN & +-inf aren't
    $LIB->_is_even($x->{value});
}

sub is_int {
    # return true when arg (BINT or num_str) is an integer
    # always true for Math::BigInt, but different for Math::BigFloat objects
    my ($class, $x) = ref($_[0]) ? (undef, $_[0]) : objectify(1, @_);

    $x->{sign} =~ /^[+-]$/ ? 1 : 0; # inf/-inf/NaN aren't
}

###############################################################################
# Comparison methods
###############################################################################

sub bcmp {
    # Compares 2 values.  Returns one of undef, <0, =0, >0. (suitable for sort)
    # (BINT or num_str, BINT or num_str) return cond_code

    # set up parameters
    my ($class, $x, $y) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                        ? (ref($_[0]), @_)
                        : objectify(2, @_);

    return $upgrade->bcmp($x, $y) if defined $upgrade &&
      ((!$x->isa($class)) || (!$y->isa($class)));

    if (($x->{sign} !~ /^[+-]$/) || ($y->{sign} !~ /^[+-]$/)) {
        # handle +-inf and NaN
        return undef if (($x->{sign} eq $nan) || ($y->{sign} eq $nan));
        return 0 if $x->{sign} eq $y->{sign} && $x->{sign} =~ /^[+-]inf$/;
        return +1 if $x->{sign} eq '+inf';
        return -1 if $x->{sign} eq '-inf';
        return -1 if $y->{sign} eq '+inf';
        return +1;
    }
    # check sign for speed first
    return 1 if $x->{sign} eq '+' && $y->{sign} eq '-'; # does also 0 <=> -y
    return -1 if $x->{sign} eq '-' && $y->{sign} eq '+'; # does also -x <=> 0

    # have same sign, so compare absolute values.  Don't make tests for zero
    # here because it's actually slower than testing in Calc (especially w/ Pari
    # et al)

    # post-normalized compare for internal use (honors signs)
    if ($x->{sign} eq '+') {
        # $x and $y both > 0
        return $LIB->_acmp($x->{value}, $y->{value});
    }

    # $x && $y both < 0
    $LIB->_acmp($y->{value}, $x->{value}); # swapped acmp (lib returns 0, 1, -1)
}

sub bacmp {
    # Compares 2 values, ignoring their signs.
    # Returns one of undef, <0, =0, >0. (suitable for sort)
    # (BINT, BINT) return cond_code

    # set up parameters
    my ($class, $x, $y) = ref($_[0]) && ref($_[0]) eq ref($_[1])
                        ? (ref($_[0]), @_)
                        : objectify(2, @_);

    return $upgrade->bacmp($x, $y) if defined $upgrade &&
      ((!$x->isa($class)) || (!$y->isa($class)));

    if (($x->{sign} !~ /^[+-]$/) || ($y->{sign} !~ /^[+-]$/)) {
        # handle +-inf and NaN
        return undef if (($x->{sign} eq $nan) || ($y->{sign} eq $nan));
        return 0 if $x->{sign} =~ /^[+-]inf$/ && $y->{sign} =~ /^[+-]inf$/;
        return 1 if $x->{sign} =~ /^[+-]inf$/ && $y->{sign} !~ /^[+-]inf$/;
        return -1;
    }
    $LIB->_acmp($x->{value}, $y->{value}); # lib does only 0, 1, -1
}

sub beq {
    my $self    = shift;
    my $selfref = ref $self;

    croak 'beq() is an instance method, not a class method' unless $selfref;
    croak 'Wrong number of arguments for beq()' unless @_ == 1;

    my $cmp = $self -> bcmp(shift);
    return defined($cmp) && ! $cmp;
}

sub bne {
    my $self    = shift;
    my $selfref = ref $self;

    croak 'bne() is an instance method, not a class method' unless $selfref;
    croak 'Wrong number of arguments for bne()' unless @_ == 1;

    my $cmp = $self -> bcmp(shift);
    return defined($cmp) && ! $cmp ? '' : 1;
}

sub blt {
    my $self    = shift;
    my $selfref = ref $self;

    croak 'blt() is an instance method, not a class method' unless $selfref;
    croak 'Wrong number of arguments for blt()' unless @_ == 1;

    my $cmp = $self -> bcmp(shift);
    return defined($cmp) && $cmp < 0;
}

sub ble {
    my $self    = shift;
    my $selfref = ref $self;

    croak 'ble() is an instance method, not a class method' unless $selfref;
    croak 'Wrong number of arguments for ble()' unless @_ == 1;

    my $cmp = $self -> bcmp(shift);
    return defined($cmp) && $cmp <= 0;
}

sub bgt {
    my $self    = shift;
    my $selfref = ref $self;

    croak 'bgt() is an instance method, not a class method' unless $selfref;
    croak 'Wrong number of arguments for bgt()' unless @_ == 1;

    my $cmp = $self -> bcmp(shift);
    return defined($cmp) && $cmp > 0;
}

sub bge {
    my $self    = shift;
    my $selfref = ref $self;

    croak 'bge() is an instance method, not a class method'
        unless $selfref;
    croak 'Wrong number of arguments for bge()' unless @_ == 1;

    my $cmp = $self -> bcmp(shift);
    return defined($cmp) && $cmp >= 0;
}

###############################################################################
# Arithmetic methods
###############################################################################

sub bneg {
    # (BINT or num_str) return BINT
    # negate number or make a negated number from string
    my ($class, $x) = ref($_[0]) ? (undef, $_[0]) : objectify(1, @_);

    return $x if $x->modify('bneg');

    # for +0 do not negate (to have always normalized +0). Does nothing for 'NaN'
    $x->{sign} =~ tr/+-/-+/ unless ($x->{sign} eq '+' && $LIB->_is_zero($x->{value}));
    $x;
}

sub babs {
    # (BINT or num_str) return BINT
    # make number absolute, or return absolute BINT from string
    my ($class, $x) = ref($_[0]) ? (undef, $_[0]) : objectify(1, @_);

    return $x if $x->modify('babs');
    # post-normalized abs for internal use (does nothing for NaN)
    $x->{sign} =~ s/^-/+/;
    $x;
}

sub bsgn {
    # Signum function.

    my $self = shift;

    return $self if $self->modify('bsgn');

    return $self -> bone("+") if $self -> is_pos();
    return $self -> bone("-") if $self -> is_neg();
    return $self;               # zero or NaN
}

sub bnorm {
    # (numstr or BINT) return BINT
    # Normalize number -- no-op here
    my ($class, $x) = ref($_[0]) ? (undef, $_[0]) : objectify(1, @_);
    $x;
}

sub binc {
    # increment arg by one
    my ($class, $x, $a, $p, $r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);
    return $x if $x->modify('binc');

    if ($x->{sign} eq '+') {
        $x->{value} = $LIB->_inc($x->{value});
        return $x->round($a, $p, $r);
    } elsif ($x->{sign} eq '-') {
        $x->{value} = $LIB->_dec($x->{value});
        $x->{sign} = '+' if $LIB->_is_zero($x->{value}); # -1 +1 => -0 => +0
        return $x->round($a, $p, $r);
    }
    # inf, nan handling etc
    $x->badd($class->bone(), $a, $p, $r); # badd does round
}

sub bdec {
    # decrement arg by one
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);
    return $x if $x->modify('bdec');

    if ($x->{sign} eq '-') {
        # x already < 0
        $x->{value} = $LIB->_inc($x->{value});
    } else {
        return $x->badd($class->bone('-'), @r)
          unless $x->{sign} eq '+'; # inf or NaN
        # >= 0
        if ($LIB->_is_zero($x->{value})) {
            # == 0
            $x->{value} = $LIB->_one();
            $x->{sign} = '-'; # 0 => -1
        } else {
            # > 0
            $x->{value} = $LIB->_dec($x->{value});
        }
    }
    $x->round(@r);
}

#sub bstrcmp {
#    my $self    = shift;
#    my $selfref = ref $self;
#    my $class   = $selfref || $self;
#
#    croak 'bstrcmp() is an instance method, not a class method'
#        unless $selfref;
#    croak 'Wrong number of arguments for bstrcmp()' unless @_ == 1;
#
#    return $self -> bstr() CORE::cmp shift;
#}
#
#sub bstreq {
#    my $self    = shift;
#    my $selfref = ref $self;
#    my $class   = $selfref || $self;
#
#    croak 'bstreq() is an instance method, not a class method'
#        unless $selfref;
#    croak 'Wrong number of arguments for bstreq()' unless @_ == 1;
#
#    my $cmp = $self -> bstrcmp(shift);
#    return defined($cmp) && ! $cmp;
#}
#
#sub bstrne {
#    my $self    = shift;
#    my $selfref = ref $self;
#    my $class   = $selfref || $self;
#
#    croak 'bstrne() is an instance method, not a class method'
#        unless $selfref;
#    croak 'Wrong number of arguments for bstrne()' unless @_ == 1;
#
#    my $cmp = $self -> bstrcmp(shift);
#    return defined($cmp) && ! $cmp ? '' : 1;
#}
#
#sub bstrlt {
#    my $self    = shift;
#    my $selfref = ref $self;
#    my $class   = $selfref || $self;
#
#    croak 'bstrlt() is an instance method, not a class method'
#        unless $selfref;
#    croak 'Wrong number of arguments for bstrlt()' unless @_ == 1;
#
#    my $cmp = $self -> bstrcmp(shift);
#    return defined($cmp) && $cmp < 0;
#}
#
#sub bstrle {
#    my $self    = shift;
#    my $selfref = ref $self;
#    my $class   = $selfref || $self;
#
#    croak 'bstrle() is an instance method, not a class method'
#        unless $selfref;
#    croak 'Wrong number of arguments for bstrle()' unless @_ == 1;
#
#    my $cmp = $self -> bstrcmp(shift);
#    return defined($cmp) && $cmp <= 0;
#}
#
#sub bstrgt {
#    my $self    = shift;
#    my $selfref = ref $self;
#    my $class   = $selfref || $self;
#
#    croak 'bstrgt() is an instance method, not a class method'
#        unless $selfref;
#    croak 'Wrong number of arguments for bstrgt()' unless @_ == 1;
#
#    my $cmp = $self -> bstrcmp(shift);
#    return defined($cmp) && $cmp > 0;
#}
#
#sub bstrge {
#    my $self    = shift;
#    my $selfref = ref $self;
#    my $class   = $selfref || $self;
#
#    croak 'bstrge() is an instance method, not a class method'
#        unless $selfref;
#    croak 'Wrong number of arguments for bstrge()' unless @_ == 1;
#
#    my $cmp = $self -> bstrcmp(shift);
#    return defined($cmp) && $cmp >= 0;
#}

sub badd {
    # add second arg (BINT or string) to first (BINT) (modifies first)
    # return result as BINT

    # set up parameters
    my ($class, $x, $y, @r) = (ref($_[0]), @_);
    # objectify is costly, so avoid it
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $x, $y, @r) = objectify(2, @_);
    }

    return $x if $x->modify('badd');
    return $upgrade->badd($upgrade->new($x), $upgrade->new($y), @r) if defined $upgrade &&
      ((!$x->isa($class)) || (!$y->isa($class)));

    $r[3] = $y;                 # no push!
    # inf and NaN handling
    if ($x->{sign} !~ /^[+-]$/ || $y->{sign} !~ /^[+-]$/) {
        # NaN first
        return $x->bnan() if (($x->{sign} eq $nan) || ($y->{sign} eq $nan));
        # inf handling
        if (($x->{sign} =~ /^[+-]inf$/) && ($y->{sign} =~ /^[+-]inf$/)) {
            # +inf++inf or -inf+-inf => same, rest is NaN
            return $x if $x->{sign} eq $y->{sign};
            return $x->bnan();
        }
        # +-inf + something => +inf
        # something +-inf => +-inf
        $x->{sign} = $y->{sign}, return $x if $y->{sign} =~ /^[+-]inf$/;
        return $x;
    }

    my ($sx, $sy) = ($x->{sign}, $y->{sign});  # get signs

    if ($sx eq $sy) {
        $x->{value} = $LIB->_add($x->{value}, $y->{value}); # same sign, abs add
    } else {
        my $a = $LIB->_acmp ($y->{value}, $x->{value}); # absolute compare
        if ($a > 0) {
            $x->{value} = $LIB->_sub($y->{value}, $x->{value}, 1); # abs sub w/ swap
            $x->{sign} = $sy;
        } elsif ($a == 0) {
            # speedup, if equal, set result to 0
            $x->{value} = $LIB->_zero();
            $x->{sign} = '+';
        } else                  # a < 0
        {
            $x->{value} = $LIB->_sub($x->{value}, $y->{value}); # abs sub
        }
    }
    $x->round(@r);
}

sub bsub {
    # (BINT or num_str, BINT or num_str) return BINT
    # subtract second arg from first, modify first

    # set up parameters
    my ($class, $x, $y, @r) = (ref($_[0]), @_);

    # objectify is costly, so avoid it
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $x, $y, @r) = objectify(2, @_);
    }

    return $x if $x -> modify('bsub');

    return $upgrade -> new($x) -> bsub($upgrade -> new($y), @r)
      if defined $upgrade && (!$x -> isa($class) || !$y -> isa($class));

    return $x -> round(@r) if $y -> is_zero();

    # To correctly handle the lone special case $x -> bsub($x), we note the
    # sign of $x, then flip the sign from $y, and if the sign of $x did change,
    # too, then we caught the special case:

    my $xsign = $x -> {sign};
    $y -> {sign} =~ tr/+-/-+/;  # does nothing for NaN
    if ($xsign ne $x -> {sign}) {
        # special case of $x -> bsub($x) results in 0
        return $x -> bzero(@r) if $xsign =~ /^[+-]$/;
        return $x -> bnan();    # NaN, -inf, +inf
    }
    $x -> badd($y, @r);         # badd does not leave internal zeros
    $y -> {sign} =~ tr/+-/-+/;  # refix $y (does nothing for NaN)
    $x;                         # already rounded by badd() or no rounding
}

sub bmul {
    # multiply the first number by the second number
    # (BINT or num_str, BINT or num_str) return BINT

    # set up parameters
    my ($class, $x, $y, @r) = (ref($_[0]), @_);
    # objectify is costly, so avoid it
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $x, $y, @r) = objectify(2, @_);
    }

    return $x if $x->modify('bmul');

    return $x->bnan() if (($x->{sign} eq $nan) || ($y->{sign} eq $nan));

    # inf handling
    if (($x->{sign} =~ /^[+-]inf$/) || ($y->{sign} =~ /^[+-]inf$/)) {
        return $x->bnan() if $x->is_zero() || $y->is_zero();
        # result will always be +-inf:
        # +inf * +/+inf => +inf, -inf * -/-inf => +inf
        # +inf * -/-inf => -inf, -inf * +/+inf => -inf
        return $x->binf() if ($x->{sign} =~ /^\+/ && $y->{sign} =~ /^\+/);
        return $x->binf() if ($x->{sign} =~ /^-/ && $y->{sign} =~ /^-/);
        return $x->binf('-');
    }

    return $upgrade->bmul($x, $upgrade->new($y), @r)
      if defined $upgrade && !$y->isa($class);

    $r[3] = $y;                 # no push here

    $x->{sign} = $x->{sign} eq $y->{sign} ? '+' : '-'; # +1 * +1 or -1 * -1 => +

    $x->{value} = $LIB->_mul($x->{value}, $y->{value}); # do actual math
    $x->{sign} = '+' if $LIB->_is_zero($x->{value});   # no -0

    $x->round(@r);
}

sub bmuladd {
    # multiply two numbers and then add the third to the result
    # (BINT or num_str, BINT or num_str, BINT or num_str) return BINT

    # set up parameters
    my ($class, $x, $y, $z, @r) = objectify(3, @_);

    return $x if $x->modify('bmuladd');

    return $x->bnan() if (($x->{sign} eq $nan) ||
                          ($y->{sign} eq $nan) ||
                          ($z->{sign} eq $nan));

    # inf handling of x and y
    if (($x->{sign} =~ /^[+-]inf$/) || ($y->{sign} =~ /^[+-]inf$/)) {
        return $x->bnan() if $x->is_zero() || $y->is_zero();
        # result will always be +-inf:
        # +inf * +/+inf => +inf, -inf * -/-inf => +inf
        # +inf * -/-inf => -inf, -inf * +/+inf => -inf
        return $x->binf() if ($x->{sign} =~ /^\+/ && $y->{sign} =~ /^\+/);
        return $x->binf() if ($x->{sign} =~ /^-/ && $y->{sign} =~ /^-/);
        return $x->binf('-');
    }
    # inf handling x*y and z
    if (($z->{sign} =~ /^[+-]inf$/)) {
        # something +-inf => +-inf
        $x->{sign} = $z->{sign}, return $x if $z->{sign} =~ /^[+-]inf$/;
    }

    return $upgrade->bmuladd($x, $upgrade->new($y), $upgrade->new($z), @r)
      if defined $upgrade && (!$y->isa($class) || !$z->isa($class) || !$x->isa($class));

    # TODO: what if $y and $z have A or P set?
    $r[3] = $z;                 # no push here

    $x->{sign} = $x->{sign} eq $y->{sign} ? '+' : '-'; # +1 * +1 or -1 * -1 => +

    $x->{value} = $LIB->_mul($x->{value}, $y->{value}); # do actual math
    $x->{sign} = '+' if $LIB->_is_zero($x->{value});   # no -0

    my ($sx, $sz) = ( $x->{sign}, $z->{sign} ); # get signs

    if ($sx eq $sz) {
        $x->{value} = $LIB->_add($x->{value}, $z->{value}); # same sign, abs add
    } else {
        my $a = $LIB->_acmp ($z->{value}, $x->{value}); # absolute compare
        if ($a > 0) {
            $x->{value} = $LIB->_sub($z->{value}, $x->{value}, 1); # abs sub w/ swap
            $x->{sign} = $sz;
        } elsif ($a == 0) {
            # speedup, if equal, set result to 0
            $x->{value} = $LIB->_zero();
            $x->{sign} = '+';
        } else                  # a < 0
        {
            $x->{value} = $LIB->_sub($x->{value}, $z->{value}); # abs sub
        }
    }
    $x->round(@r);
}

sub bdiv {
    # This does floored division, where the quotient is floored, i.e., rounded
    # towards negative infinity. As a consequence, the remainder has the same
    # sign as the divisor.

    # Set up parameters.
    my ($class, $x, $y, @r) = (ref($_[0]), @_);

    # objectify() is costly, so avoid it if we can.
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $x, $y, @r) = objectify(2, @_);
    }

    return $x if $x -> modify('bdiv');

    my $wantarray = wantarray;          # call only once

    # At least one argument is NaN. Return NaN for both quotient and the
    # modulo/remainder.

    if ($x -> is_nan() || $y -> is_nan()) {
        return $wantarray ? ($x -> bnan(), $class -> bnan()) : $x -> bnan();
    }

    # Divide by zero and modulo zero.
    #
    # Division: Use the common convention that x / 0 is inf with the same sign
    # as x, except when x = 0, where we return NaN. This is also what earlier
    # versions did.
    #
    # Modulo: In modular arithmetic, the congruence relation z = x (mod y)
    # means that there is some integer k such that z - x = k y. If y = 0, we
    # get z - x = 0 or z = x. This is also what earlier versions did, except
    # that 0 % 0 returned NaN.
    #
    #     inf /    0 =  inf                  inf %    0 =  inf
    #       5 /    0 =  inf                    5 %    0 =    5
    #       0 /    0 =  NaN                    0 %    0 =    0
    #      -5 /    0 = -inf                   -5 %    0 =   -5
    #    -inf /    0 = -inf                 -inf %    0 = -inf

    if ($y -> is_zero()) {
        my $rem;
        if ($wantarray) {
            $rem = $x -> copy();
        }
        if ($x -> is_zero()) {
            $x -> bnan();
        } else {
            $x -> binf($x -> {sign});
        }
        return $wantarray ? ($x, $rem) : $x;
    }

    # Numerator (dividend) is +/-inf, and denominator is finite and non-zero.
    # The divide by zero cases are covered above. In all of the cases listed
    # below we return the same as core Perl.
    #
    #     inf / -inf =  NaN                  inf % -inf =  NaN
    #     inf /   -5 = -inf                  inf %   -5 =  NaN
    #     inf /    5 =  inf                  inf %    5 =  NaN
    #     inf /  inf =  NaN                  inf %  inf =  NaN
    #
    #    -inf / -inf =  NaN                 -inf % -inf =  NaN
    #    -inf /   -5 =  inf                 -inf %   -5 =  NaN
    #    -inf /    5 = -inf                 -inf %    5 =  NaN
    #    -inf /  inf =  NaN                 -inf %  inf =  NaN

    if ($x -> is_inf()) {
        my $rem;
        $rem = $class -> bnan() if $wantarray;
        if ($y -> is_inf()) {
            $x -> bnan();
        } else {
            my $sign = $x -> bcmp(0) == $y -> bcmp(0) ? '+' : '-';
            $x -> binf($sign);
        }
        return $wantarray ? ($x, $rem) : $x;
    }

    # Denominator (divisor) is +/-inf. The cases when the numerator is +/-inf
    # are covered above. In the modulo cases (in the right column) we return
    # the same as core Perl, which does floored division, so for consistency we
    # also do floored division in the division cases (in the left column).
    #
    #      -5 /  inf =   -1                   -5 %  inf =  inf
    #       0 /  inf =    0                    0 %  inf =    0
    #       5 /  inf =    0                    5 %  inf =    5
    #
    #      -5 / -inf =    0                   -5 % -inf =   -5
    #       0 / -inf =    0                    0 % -inf =    0
    #       5 / -inf =   -1                    5 % -inf = -inf

    if ($y -> is_inf()) {
        my $rem;
        if ($x -> is_zero() || $x -> bcmp(0) == $y -> bcmp(0)) {
            $rem = $x -> copy() if $wantarray;
            $x -> bzero();
        } else {
            $rem = $class -> binf($y -> {sign}) if $wantarray;
            $x -> bone('-');
        }
        return $wantarray ? ($x, $rem) : $x;
    }

    # At this point, both the numerator and denominator are finite numbers, and
    # the denominator (divisor) is non-zero.

    return $upgrade -> bdiv($upgrade -> new($x), $upgrade -> new($y), @r)
      if defined $upgrade;

    $r[3] = $y;                                   # no push!

    # Inialize remainder.

    my $rem = $class -> bzero();

    # Are both operands the same object, i.e., like $x -> bdiv($x)? If so,
    # flipping the sign of $y also flips the sign of $x.

    my $xsign = $x -> {sign};
    my $ysign = $y -> {sign};

    $y -> {sign} =~ tr/+-/-+/;            # Flip the sign of $y, and see ...
    my $same = $xsign ne $x -> {sign};    # ... if that changed the sign of $x.
    $y -> {sign} = $ysign;                # Re-insert the original sign.

    if ($same) {
        $x -> bone();
    } else {
        ($x -> {value}, $rem -> {value}) =
          $LIB -> _div($x -> {value}, $y -> {value});

        if ($LIB -> _is_zero($rem -> {value})) {
            if ($xsign eq $ysign || $LIB -> _is_zero($x -> {value})) {
                $x -> {sign} = '+';
            } else {
                $x -> {sign} = '-';
            }
        } else {
            if ($xsign eq $ysign) {
                $x -> {sign} = '+';
            } else {
                if ($xsign eq '+') {
                    $x -> badd(1);
                } else {
                    $x -> bsub(1);
                }
                $x -> {sign} = '-';
            }
        }
    }

    $x -> round(@r);

    if ($wantarray) {
        unless ($LIB -> _is_zero($rem -> {value})) {
            if ($xsign ne $ysign) {
                $rem = $y -> copy() -> babs() -> bsub($rem);
            }
            $rem -> {sign} = $ysign;
        }
        $rem -> {_a} = $x -> {_a};
        $rem -> {_p} = $x -> {_p};
        $rem -> round(@r);
        return ($x, $rem);
    }

    return $x;
}

sub btdiv {
    # This does truncated division, where the quotient is truncted, i.e.,
    # rounded towards zero.
    #
    # ($q, $r) = $x -> btdiv($y) returns $q and $r so that $q is int($x / $y)
    # and $q * $y + $r = $x.

    # Set up parameters
    my ($class, $x, $y, @r) = (ref($_[0]), @_);

    # objectify is costly, so avoid it if we can.
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $x, $y, @r) = objectify(2, @_);
    }

    return $x if $x -> modify('btdiv');

    my $wantarray = wantarray;          # call only once

    # At least one argument is NaN. Return NaN for both quotient and the
    # modulo/remainder.

    if ($x -> is_nan() || $y -> is_nan()) {
        return $wantarray ? ($x -> bnan(), $class -> bnan()) : $x -> bnan();
    }

    # Divide by zero and modulo zero.
    #
    # Division: Use the common convention that x / 0 is inf with the same sign
    # as x, except when x = 0, where we return NaN. This is also what earlier
    # versions did.
    #
    # Modulo: In modular arithmetic, the congruence relation z = x (mod y)
    # means that there is some integer k such that z - x = k y. If y = 0, we
    # get z - x = 0 or z = x. This is also what earlier versions did, except
    # that 0 % 0 returned NaN.
    #
    #     inf / 0 =  inf                     inf % 0 =  inf
    #       5 / 0 =  inf                       5 % 0 =    5
    #       0 / 0 =  NaN                       0 % 0 =    0
    #      -5 / 0 = -inf                      -5 % 0 =   -5
    #    -inf / 0 = -inf                    -inf % 0 = -inf

    if ($y -> is_zero()) {
        my $rem;
        if ($wantarray) {
            $rem = $x -> copy();
        }
        if ($x -> is_zero()) {
            $x -> bnan();
        } else {
            $x -> binf($x -> {sign});
        }
        return $wantarray ? ($x, $rem) : $x;
    }

    # Numerator (dividend) is +/-inf, and denominator is finite and non-zero.
    # The divide by zero cases are covered above. In all of the cases listed
    # below we return the same as core Perl.
    #
    #     inf / -inf =  NaN                  inf % -inf =  NaN
    #     inf /   -5 = -inf                  inf %   -5 =  NaN
    #     inf /    5 =  inf                  inf %    5 =  NaN
    #     inf /  inf =  NaN                  inf %  inf =  NaN
    #
    #    -inf / -inf =  NaN                 -inf % -inf =  NaN
    #    -inf /   -5 =  inf                 -inf %   -5 =  NaN
    #    -inf /    5 = -inf                 -inf %    5 =  NaN
    #    -inf /  inf =  NaN                 -inf %  inf =  NaN

    if ($x -> is_inf()) {
        my $rem;
        $rem = $class -> bnan() if $wantarray;
        if ($y -> is_inf()) {
            $x -> bnan();
        } else {
            my $sign = $x -> bcmp(0) == $y -> bcmp(0) ? '+' : '-';
            $x -> binf($sign);
        }
        return $wantarray ? ($x, $rem) : $x;
    }

    # Denominator (divisor) is +/-inf. The cases when the numerator is +/-inf
    # are covered above. In the modulo cases (in the right column) we return
    # the same as core Perl, which does floored division, so for consistency we
    # also do floored division in the division cases (in the left column).
    #
    #      -5 /  inf =    0                   -5 %  inf =  -5
    #       0 /  inf =    0                    0 %  inf =   0
    #       5 /  inf =    0                    5 %  inf =   5
    #
    #      -5 / -inf =    0                   -5 % -inf =  -5
    #       0 / -inf =    0                    0 % -inf =   0
    #       5 / -inf =    0                    5 % -inf =   5

    if ($y -> is_inf()) {
        my $rem;
        $rem = $x -> copy() if $wantarray;
        $x -> bzero();
        return $wantarray ? ($x, $rem) : $x;
    }

    return $upgrade -> btdiv($upgrade -> new($x), $upgrade -> new($y), @r)
      if defined $upgrade;

    $r[3] = $y;                 # no push!

    # Inialize remainder.

    my $rem = $class -> bzero();

    # Are both operands the same object, i.e., like $x -> bdiv($x)? If so,
    # flipping the sign of $y also flips the sign of $x.

    my $xsign = $x -> {sign};
    my $ysign = $y -> {sign};

    $y -> {sign} =~ tr/+-/-+/;            # Flip the sign of $y, and see ...
    my $same = $xsign ne $x -> {sign};    # ... if that changed the sign of $x.
    $y -> {sign} = $ysign;                # Re-insert the original sign.

    if ($same) {
        $x -> bone();
    } else {
        ($x -> {value}, $rem -> {value}) =
          $LIB -> _div($x -> {value}, $y -> {value});

        $x -> {sign} = $xsign eq $ysign ? '+' : '-';
        $x -> {sign} = '+' if $LIB -> _is_zero($x -> {value});
        $x -> round(@r);
    }

    if (wantarray) {
        $rem -> {sign} = $xsign;
        $rem -> {sign} = '+' if $LIB -> _is_zero($rem -> {value});
        $rem -> {_a} = $x -> {_a};
        $rem -> {_p} = $x -> {_p};
        $rem -> round(@r);
        return ($x, $rem);
    }

    return $x;
}

sub bmod {
    # This is the remainder after floored division.

    # Set up parameters.
    my ($class, $x, $y, @r) = (ref($_[0]), @_);

    # objectify is costly, so avoid it
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $x, $y, @r) = objectify(2, @_);
    }

    return $x if $x -> modify('bmod');
    $r[3] = $y;                 # no push!

    # At least one argument is NaN.

    if ($x -> is_nan() || $y -> is_nan()) {
        return $x -> bnan();
    }

    # Modulo zero. See documentation for bdiv().

    if ($y -> is_zero()) {
        return $x;
    }

    # Numerator (dividend) is +/-inf.

    if ($x -> is_inf()) {
        return $x -> bnan();
    }

    # Denominator (divisor) is +/-inf.

    if ($y -> is_inf()) {
        if ($x -> is_zero() || $x -> bcmp(0) == $y -> bcmp(0)) {
            return $x;
        } else {
            return $x -> binf($y -> sign());
        }
    }

    # Calc new sign and in case $y == +/- 1, return $x.

    $x -> {value} = $LIB -> _mod($x -> {value}, $y -> {value});
    if ($LIB -> _is_zero($x -> {value})) {
        $x -> {sign} = '+';     # do not leave -0
    } else {
        $x -> {value} = $LIB -> _sub($y -> {value}, $x -> {value}, 1) # $y-$x
          if ($x -> {sign} ne $y -> {sign});
        $x -> {sign} = $y -> {sign};
    }

    $x -> round(@r);
}

sub btmod {
    # Remainder after truncated division.

    # set up parameters
    my ($class, $x, $y, @r) = (ref($_[0]), @_);

    # objectify is costly, so avoid it
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $x, $y, @r) = objectify(2, @_);
    }

    return $x if $x -> modify('btmod');

    # At least one argument is NaN.

    if ($x -> is_nan() || $y -> is_nan()) {
        return $x -> bnan();
    }

    # Modulo zero. See documentation for btdiv().

    if ($y -> is_zero()) {
        return $x;
    }

    # Numerator (dividend) is +/-inf.

    if ($x -> is_inf()) {
        return $x -> bnan();
    }

    # Denominator (divisor) is +/-inf.

    if ($y -> is_inf()) {
        return $x;
    }

    return $upgrade -> btmod($upgrade -> new($x), $upgrade -> new($y), @r)
      if defined $upgrade;

    $r[3] = $y;                 # no push!

    my $xsign = $x -> {sign};

    $x -> {value} = $LIB -> _mod($x -> {value}, $y -> {value});

    $x -> {sign} = $xsign;
    $x -> {sign} = '+' if $LIB -> _is_zero($x -> {value});
    $x -> round(@r);
    return $x;
}

sub bmodinv {
    # Return modular multiplicative inverse:
    #
    #   z is the modular inverse of x (mod y) if and only if
    #
    #       x*z ≡ 1  (mod y)
    #
    # If the modulus y is larger than one, x and z are relative primes (i.e.,
    # their greatest common divisor is one).
    #
    # If no modular multiplicative inverse exists, NaN is returned.

    # set up parameters
    my ($class, $x, $y, @r) = (undef, @_);
    # objectify is costly, so avoid it
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $x, $y, @r) = objectify(2, @_);
    }

    return $x if $x->modify('bmodinv');

    # Return NaN if one or both arguments is +inf, -inf, or nan.

    return $x->bnan() if ($y->{sign} !~ /^[+-]$/ ||
                          $x->{sign} !~ /^[+-]$/);

    # Return NaN if $y is zero; 1 % 0 makes no sense.

    return $x->bnan() if $y->is_zero();

    # Return 0 in the trivial case. $x % 1 or $x % -1 is zero for all finite
    # integers $x.

    return $x->bzero() if ($y->is_one() ||
                           $y->is_one('-'));

    # Return NaN if $x = 0, or $x modulo $y is zero. The only valid case when
    # $x = 0 is when $y = 1 or $y = -1, but that was covered above.
    #
    # Note that computing $x modulo $y here affects the value we'll feed to
    # $LIB->_modinv() below when $x and $y have opposite signs. E.g., if $x =
    # 5 and $y = 7, those two values are fed to _modinv(), but if $x = -5 and
    # $y = 7, the values fed to _modinv() are $x = 2 (= -5 % 7) and $y = 7.
    # The value if $x is affected only when $x and $y have opposite signs.

    $x->bmod($y);
    return $x->bnan() if $x->is_zero();

    # Compute the modular multiplicative inverse of the absolute values. We'll
    # correct for the signs of $x and $y later. Return NaN if no GCD is found.

    ($x->{value}, $x->{sign}) = $LIB->_modinv($x->{value}, $y->{value});
    return $x->bnan() if !defined $x->{value};

    # Library inconsistency workaround: _modinv() in Math::BigInt::GMP versions
    # <= 1.32 return undef rather than a "+" for the sign.

    $x->{sign} = '+' unless defined $x->{sign};

    # When one or both arguments are negative, we have the following
    # relations.  If x and y are positive:
    #
    #   modinv(-x, -y) = -modinv(x, y)
    #   modinv(-x, y) = y - modinv(x, y)  = -modinv(x, y) (mod y)
    #   modinv( x, -y) = modinv(x, y) - y  =  modinv(x, y) (mod -y)

    # We must swap the sign of the result if the original $x is negative.
    # However, we must compensate for ignoring the signs when computing the
    # inverse modulo. The net effect is that we must swap the sign of the
    # result if $y is negative.

    $x -> bneg() if $y->{sign} eq '-';

    # Compute $x modulo $y again after correcting the sign.

    $x -> bmod($y) if $x->{sign} ne $y->{sign};

    return $x;
}

sub bmodpow {
    # Modular exponentiation. Raises a very large number to a very large exponent
    # in a given very large modulus quickly, thanks to binary exponentiation.
    # Supports negative exponents.
    my ($class, $num, $exp, $mod, @r) = objectify(3, @_);

    return $num if $num->modify('bmodpow');

    # When the exponent 'e' is negative, use the following relation, which is
    # based on finding the multiplicative inverse 'd' of 'b' modulo 'm':
    #
    #    b^(-e) (mod m) = d^e (mod m) where b*d = 1 (mod m)

    $num->bmodinv($mod) if ($exp->{sign} eq '-');

    # Check for valid input. All operands must be finite, and the modulus must be
    # non-zero.

    return $num->bnan() if ($num->{sign} =~ /NaN|inf/ || # NaN, -inf, +inf
                            $exp->{sign} =~ /NaN|inf/ || # NaN, -inf, +inf
                            $mod->{sign} =~ /NaN|inf/);  # NaN, -inf, +inf

    # Modulo zero. See documentation for Math::BigInt's bmod() method.

    if ($mod -> is_zero()) {
        if ($num -> is_zero()) {
            return $class -> bnan();
        } else {
            return $num -> copy();
        }
    }

    # Compute 'a (mod m)', ignoring the signs on 'a' and 'm'. If the resulting
    # value is zero, the output is also zero, regardless of the signs on 'a' and
    # 'm'.

    my $value = $LIB->_modpow($num->{value}, $exp->{value}, $mod->{value});
    my $sign  = '+';

    # If the resulting value is non-zero, we have four special cases, depending
    # on the signs on 'a' and 'm'.

    unless ($LIB->_is_zero($value)) {

        # There is a negative sign on 'a' (= $num**$exp) only if the number we
        # are exponentiating ($num) is negative and the exponent ($exp) is odd.

        if ($num->{sign} eq '-' && $exp->is_odd()) {

            # When both the number 'a' and the modulus 'm' have a negative sign,
            # use this relation:
            #
            #    -a (mod -m) = -(a (mod m))

            if ($mod->{sign} eq '-') {
                $sign = '-';
            }

            # When only the number 'a' has a negative sign, use this relation:
            #
            #    -a (mod m) = m - (a (mod m))

            else {
                # Use copy of $mod since _sub() modifies the first argument.
                my $mod = $LIB->_copy($mod->{value});
                $value = $LIB->_sub($mod, $value);
                $sign  = '+';
            }

        } else {

            # When only the modulus 'm' has a negative sign, use this relation:
            #
            #    a (mod -m) = (a (mod m)) - m
            #               = -(m - (a (mod m)))

            if ($mod->{sign} eq '-') {
                # Use copy of $mod since _sub() modifies the first argument.
                my $mod = $LIB->_copy($mod->{value});
                $value = $LIB->_sub($mod, $value);
                $sign  = '-';
            }

            # When neither the number 'a' nor the modulus 'm' have a negative
            # sign, directly return the already computed value.
            #
            #    (a (mod m))

        }

    }

    $num->{value} = $value;
    $num->{sign}  = $sign;

    return $num -> round(@r);
}

sub bpow {
    # (BINT or num_str, BINT or num_str) return BINT
    # compute power of two numbers -- stolen from Knuth Vol 2 pg 233
    # modifies first argument

    # set up parameters
    my ($class, $x, $y, @r) = (ref($_[0]), @_);
    # objectify is costly, so avoid it
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $x, $y, @r) = objectify(2, @_);
    }

    return $x if $x->modify('bpow');

    # $x and/or $y is a NaN
    return $x->bnan() if $x->is_nan() || $y->is_nan();

    # $x and/or $y is a +/-Inf
    if ($x->is_inf("-")) {
        return $x->bzero()   if $y->is_negative();
        return $x->bnan()    if $y->is_zero();
        return $x            if $y->is_odd();
        return $x->bneg();
    } elsif ($x->is_inf("+")) {
        return $x->bzero()   if $y->is_negative();
        return $x->bnan()    if $y->is_zero();
        return $x;
    } elsif ($y->is_inf("-")) {
        return $x->bnan()    if $x -> is_one("-");
        return $x->binf("+") if $x -> is_zero();
        return $x->bone()    if $x -> is_one("+");
        return $x->bzero();
    } elsif ($y->is_inf("+")) {
        return $x->bnan()    if $x -> is_one("-");
        return $x->bzero()   if $x -> is_zero();
        return $x->bone()    if $x -> is_one("+");
        return $x->binf("+");
    }

    return $upgrade->bpow($upgrade->new($x), $y, @r)
      if defined $upgrade && (!$y->isa($class) || $y->{sign} eq '-');

    $r[3] = $y;                 # no push!

    # 0 ** -y => ( 1 / (0 ** y)) => 1 / 0 => +inf
    return $x->binf() if $y->is_negative() && $x->is_zero();

    # 1 ** -y => 1 / (1 ** |y|)
    return $x->bzero() if $y->is_negative() && !$LIB->_is_one($x->{value});

    $x->{value} = $LIB->_pow($x->{value}, $y->{value});
    $x->{sign}  = $x->is_negative() && $y->is_odd() ? '-' : '+';
    $x->round(@r);
}

sub blog {
    # Return the logarithm of the operand. If a second operand is defined, that
    # value is used as the base, otherwise the base is assumed to be Euler's
    # constant.

    my ($class, $x, $base, @r);

    # Don't objectify the base, since an undefined base, as in $x->blog() or
    # $x->blog(undef) signals that the base is Euler's number.

    if (!ref($_[0]) && $_[0] =~ /^[A-Za-z]|::/) {
        # E.g., Math::BigInt->blog(256, 2)
        ($class, $x, $base, @r) =
          defined $_[2] ? objectify(2, @_) : objectify(1, @_);
    } else {
        # E.g., Math::BigInt::blog(256, 2) or $x->blog(2)
        ($class, $x, $base, @r) =
          defined $_[1] ? objectify(2, @_) : objectify(1, @_);
    }

    return $x if $x->modify('blog');

    # Handle all exception cases and all trivial cases. I have used Wolfram
    # Alpha (http://www.wolframalpha.com) as the reference for these cases.

    return $x -> bnan() if $x -> is_nan();

    if (defined $base) {
        $base = $class -> new($base) unless ref $base;
        if ($base -> is_nan() || $base -> is_one()) {
            return $x -> bnan();
        } elsif ($base -> is_inf() || $base -> is_zero()) {
            return $x -> bnan() if $x -> is_inf() || $x -> is_zero();
            return $x -> bzero();
        } elsif ($base -> is_negative()) {        # -inf < base < 0
            return $x -> bzero() if $x -> is_one(); #     x = 1
            return $x -> bone()  if $x == $base;    #     x = base
            return $x -> bnan();                    #     otherwise
        }
        return $x -> bone() if $x == $base; # 0 < base && 0 < x < inf
    }

    # We now know that the base is either undefined or >= 2 and finite.

    return $x -> binf('+') if $x -> is_inf(); #   x = +/-inf
    return $x -> bnan()    if $x -> is_neg(); #   -inf < x < 0
    return $x -> bzero()   if $x -> is_one(); #   x = 1
    return $x -> binf('-') if $x -> is_zero(); #   x = 0

    # At this point we are done handling all exception cases and trivial cases.

    return $upgrade -> blog($upgrade -> new($x), $base, @r) if defined $upgrade;

    # fix for bug #24969:
    # the default base is e (Euler's number) which is not an integer
    if (!defined $base) {
        require Math::BigFloat;
        my $u = Math::BigFloat->blog(Math::BigFloat->new($x))->as_int();
        # modify $x in place
        $x->{value} = $u->{value};
        $x->{sign} = $u->{sign};
        return $x;
    }

    my ($rc) = $LIB->_log_int($x->{value}, $base->{value});
    return $x->bnan() unless defined $rc; # not possible to take log?
    $x->{value} = $rc;
    $x->round(@r);
}

sub bexp {
    # Calculate e ** $x (Euler's number to the power of X), truncated to
    # an integer value.
    my ($class, $x, @r) = ref($_[0]) ? (ref($_[0]), @_) : objectify(1, @_);
    return $x if $x->modify('bexp');

    # inf, -inf, NaN, <0 => NaN
    return $x->bnan() if $x->{sign} eq 'NaN';
    return $x->bone() if $x->is_zero();
    return $x if $x->{sign} eq '+inf';
    return $x->bzero() if $x->{sign} eq '-inf';

    my $u;
    {
        # run through Math::BigFloat unless told otherwise
        require Math::BigFloat unless defined $upgrade;
        local $upgrade = 'Math::BigFloat' unless defined $upgrade;
        # calculate result, truncate it to integer
        $u = $upgrade->bexp($upgrade->new($x), @r);
    }

    if (defined $upgrade) {
        $x = $u;
    } else {
        $u = $u->as_int();
        # modify $x in place
        $x->{value} = $u->{value};
        $x->round(@r);
    }
}

sub bnok {
    # Calculate n over k (binomial coefficient or "choose" function) as
    # integer.

    # Set up parameters.
    my ($self, $n, $k, @r) = (ref($_[0]), @_);

    # Objectify is costly, so avoid it.
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($self, $n, $k, @r) = objectify(2, @_);
    }

    return $n if $n->modify('bnok');

    # All cases where at least one argument is NaN.

    return $n->bnan() if $n->{sign} eq 'NaN' || $k->{sign} eq 'NaN';

    # All cases where at least one argument is +/-inf.

    if ($n -> is_inf()) {
        if ($k -> is_inf()) {                   # bnok(+/-inf,+/-inf)
            return $n -> bnan();
        } elsif ($k -> is_neg()) {              # bnok(+/-inf,k), k < 0
            return $n -> bzero();
        } elsif ($k -> is_zero()) {             # bnok(+/-inf,k), k = 0
            return $n -> bone();
        } else {
            if ($n -> is_inf("+")) {            # bnok(+inf,k), 0 < k < +inf
                return $n -> binf("+");
            } else {                            # bnok(-inf,k), k > 0
                my $sign = $k -> is_even() ? "+" : "-";
                return $n -> binf($sign);
            }
        }
    }

    elsif ($k -> is_inf()) {            # bnok(n,+/-inf), -inf <= n <= inf
        return $n -> bnan();
    }

    # At this point, both n and k are real numbers.

    my $sign = 1;

    if ($n >= 0) {
        if ($k < 0 || $k > $n) {
            return $n -> bzero();
        }
    } else {

        if ($k >= 0) {

            # n < 0 and k >= 0: bnok(n,k) = (-1)^k * bnok(-n+k-1,k)

            $sign = (-1) ** $k;
            $n -> bneg() -> badd($k) -> bdec();

        } elsif ($k <= $n) {

            # n < 0 and k <= n: bnok(n,k) = (-1)^(n-k) * bnok(-k-1,n-k)

            $sign = (-1) ** ($n - $k);
            my $x0 = $n -> copy();
            $n -> bone() -> badd($k) -> bneg();
            $k = $k -> copy();
            $k -> bneg() -> badd($x0);

        } else {

            # n < 0 and n < k < 0:

            return $n -> bzero();
        }
    }

    $n->{value} = $LIB->_nok($n->{value}, $k->{value});
    $n -> bneg() if $sign == -1;

    $n->round(@r);
}

sub buparrow {
    my $a = shift;
    my $y = $a -> uparrow(@_);
    $a -> {value} = $y -> {value};
    return $a;
}

sub uparrow {
    # Knuth's up-arrow notation buparrow(a, n, b)
    #
    # The following is a simple, recursive implementation of the up-arrow
    # notation, just to show the idea. Such implementations cause "Deep
    # recursion on subroutine ..." warnings, so we use a faster, non-recursive
    # algorithm below with @_ as a stack.
    #
    #   sub buparrow {
    #       my ($a, $n, $b) = @_;
    #       return $a ** $b if $n == 1;
    #       return $a * $b  if $n == 0;
    #       return 1        if $b == 0;
    #       return buparrow($a, $n - 1, buparrow($a, $n, $b - 1));
    #   }

    my ($a, $b, $n) = @_;
    my $class = ref $a;
    croak("a must be non-negative") if $a < 0;
    croak("n must be non-negative") if $n < 0;
    croak("b must be non-negative") if $b < 0;

    while (@_ >= 3) {

        # return $a ** $b if $n == 1;

        if ($_[-2] == 1) {
            my ($a, $n, $b) = splice @_, -3;
            push @_, $a ** $b;
            next;
        }

        # return $a * $b if $n == 0;

        if ($_[-2] == 0) {
            my ($a, $n, $b) = splice @_, -3;
            push @_, $a * $b;
            next;
        }

        # return 1 if $b == 0;

        if ($_[-1] == 0) {
            splice @_, -3;
            push @_, $class -> bone();
            next;
        }

        # return buparrow($a, $n - 1, buparrow($a, $n, $b - 1));

        my ($a, $n, $b) = splice @_, -3;
        push @_, ($a, $n - 1,
                      $a, $n, $b - 1);

    }

    pop @_;
}

sub backermann {
    my $m = shift;
    my $y = $m -> ackermann(@_);
    $m -> {value} = $y -> {value};
    return $m;
}

sub ackermann {
    # Ackermann's function ackermann(m, n)
    #
    # The following is a simple, recursive implementation of the ackermann
    # function, just to show the idea. Such implementations cause "Deep
    # recursion on subroutine ..." warnings, so we use a faster, non-recursive
    # algorithm below with @_ as a stack.
    #
    # sub ackermann {
    #     my ($m, $n) = @_;
    #     return $n + 1                                  if $m == 0;
    #     return ackermann($m - 1, 1)                    if $m > 0 && $n == 0;
    #     return ackermann($m - 1, ackermann($m, $n - 1) if $m > 0 && $n > 0;
    # }

    my ($m, $n) = @_;
    my $class = ref $m;
    croak("m must be non-negative") if $m < 0;
    croak("n must be non-negative") if $n < 0;

    my $two      = $class -> new("2");
    my $three    = $class -> new("3");
    my $thirteen = $class -> new("13");

    $n = pop;
    $n = $class -> new($n) unless ref($n);
    while (@_) {
        my $m = pop;
        if ($m > $three) {
            push @_, (--$m) x $n;
            while (--$m >= $three) {
                push @_, $m;
            }
            $n = $thirteen;
        } elsif ($m == $three) {
            $n = $class -> bone() -> blsft($n + $three) -> bsub($three);
        } elsif ($m == $two) {
            $n -> bmul($two) -> badd($three);
        } elsif ($m >= 0) {
            $n -> badd($m) -> binc();
        } else {
            die "negative m!";
        }
    }
    $n;
}

sub bsin {
    # Calculate sinus(x) to N digits. Unless upgrading is in effect, returns the
    # result truncated to an integer.
    my ($class, $x, @r) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return $x if $x->modify('bsin');

    return $x->bnan() if $x->{sign} !~ /^[+-]\z/; # -inf +inf or NaN => NaN

    return $upgrade->new($x)->bsin(@r) if defined $upgrade;

    require Math::BigFloat;
    # calculate the result and truncate it to integer
    my $t = Math::BigFloat->new($x)->bsin(@r)->as_int();

    $x->bone() if $t->is_one();
    $x->bzero() if $t->is_zero();
    $x->round(@r);
}

sub bcos {
    # Calculate cosinus(x) to N digits. Unless upgrading is in effect, returns the
    # result truncated to an integer.
    my ($class, $x, @r) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return $x if $x->modify('bcos');

    return $x->bnan() if $x->{sign} !~ /^[+-]\z/; # -inf +inf or NaN => NaN

    return $upgrade->new($x)->bcos(@r) if defined $upgrade;

    require Math::BigFloat;
    # calculate the result and truncate it to integer
    my $t = Math::BigFloat->new($x)->bcos(@r)->as_int();

    $x->bone() if $t->is_one();
    $x->bzero() if $t->is_zero();
    $x->round(@r);
}

sub batan {
    # Calculate arcus tangens of x to N digits. Unless upgrading is in effect, returns the
    # result truncated to an integer.
    my ($class, $x, @r) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return $x if $x->modify('batan');

    return $x->bnan() if $x->{sign} !~ /^[+-]\z/; # -inf +inf or NaN => NaN

    return $upgrade->new($x)->batan(@r) if defined $upgrade;

    # calculate the result and truncate it to integer
    my $tmp = Math::BigFloat->new($x)->batan(@r);

    $x->{value} = $LIB->_new($tmp->as_int()->bstr());
    $x->round(@r);
}

sub batan2 {
    # calculate arcus tangens of ($y/$x)

    # set up parameters
    my ($class, $y, $x, @r) = (ref($_[0]), @_);
    # objectify is costly, so avoid it
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $y, $x, @r) = objectify(2, @_);
    }

    return $y if $y->modify('batan2');

    return $y->bnan() if ($y->{sign} eq $nan) || ($x->{sign} eq $nan);

    # Y    X
    # != 0 -inf result is +- pi
    if ($x->is_inf() || $y->is_inf()) {
        # upgrade to Math::BigFloat etc.
        return $upgrade->new($y)->batan2($upgrade->new($x), @r) if defined $upgrade;
        if ($y->is_inf()) {
            if ($x->{sign} eq '-inf') {
                # calculate 3 pi/4 => 2.3.. => 2
                $y->bone(substr($y->{sign}, 0, 1));
                $y->bmul($class->new(2));
            } elsif ($x->{sign} eq '+inf') {
                # calculate pi/4 => 0.7 => 0
                $y->bzero();
            } else {
                # calculate pi/2 => 1.5 => 1
                $y->bone(substr($y->{sign}, 0, 1));
            }
        } else {
            if ($x->{sign} eq '+inf') {
                # calculate pi/4 => 0.7 => 0
                $y->bzero();
            } else {
                # PI => 3.1415.. => 3
                $y->bone(substr($y->{sign}, 0, 1));
                $y->bmul($class->new(3));
            }
        }
        return $y;
    }

    return $upgrade->new($y)->batan2($upgrade->new($x), @r) if defined $upgrade;

    require Math::BigFloat;
    my $r = Math::BigFloat->new($y)
      ->batan2(Math::BigFloat->new($x), @r)
        ->as_int();

    $x->{value} = $r->{value};
    $x->{sign} = $r->{sign};

    $x;
}

sub bsqrt {
    # calculate square root of $x
    my ($class, $x, @r) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return $x if $x->modify('bsqrt');

    return $x->bnan() if $x->{sign} !~ /^\+/; # -x or -inf or NaN => NaN
    return $x if $x->{sign} eq '+inf';        # sqrt(+inf) == inf

    return $upgrade->bsqrt($x, @r) if defined $upgrade;

    $x->{value} = $LIB->_sqrt($x->{value});
    $x->round(@r);
}

sub broot {
    # calculate $y'th root of $x

    # set up parameters
    my ($class, $x, $y, @r) = (ref($_[0]), @_);

    $y = $class->new(2) unless defined $y;

    # objectify is costly, so avoid it
    if ((!ref($x)) || (ref($x) ne ref($y))) {
        ($class, $x, $y, @r) = objectify(2, $class || $class, @_);
    }

    return $x if $x->modify('broot');

    # NaN handling: $x ** 1/0, x or y NaN, or y inf/-inf or y == 0
    return $x->bnan() if $x->{sign} !~ /^\+/ || $y->is_zero() ||
      $y->{sign} !~ /^\+$/;

    return $x->round(@r)
      if $x->is_zero() || $x->is_one() || $x->is_inf() || $y->is_one();

    return $upgrade->new($x)->broot($upgrade->new($y), @r) if defined $upgrade;

    $x->{value} = $LIB->_root($x->{value}, $y->{value});
    $x->round(@r);
}

sub bfac {
    # (BINT or num_str, BINT or num_str) return BINT
    # compute factorial number from $x, modify $x in place
    my ($class, $x, @r) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return $x if $x->modify('bfac') || $x->{sign} eq '+inf'; # inf => inf
    return $x->bnan() if $x->{sign} ne '+'; # NaN, <0 etc => NaN

    $x->{value} = $LIB->_fac($x->{value});
    $x->round(@r);
}

sub bdfac {
    # compute double factorial, modify $x in place
    my ($class, $x, @r) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return $x if $x->modify('bdfac') || $x->{sign} eq '+inf'; # inf => inf
    return $x->bnan() if $x->{sign} ne '+'; # NaN, <0 etc => NaN

    croak("bdfac() requires a newer version of the $LIB library.")
        unless $LIB->can('_dfac');

    $x->{value} = $LIB->_dfac($x->{value});
    $x->round(@r);
}

sub bfib {
    # compute Fibonacci number(s)
    my ($class, $x, @r) = objectify(1, @_);

    croak("bfib() requires a newer version of the $LIB library.")
        unless $LIB->can('_fib');

    return $x if $x->modify('bfib');

    # List context.

    if (wantarray) {
        return () if $x ->  is_nan();
        croak("bfib() can't return an infinitely long list of numbers")
            if $x -> is_inf();

        # Use the backend library to compute the first $x Fibonacci numbers.

        my @values = $LIB->_fib($x->{value});

        # Make objects out of them. The last element in the array is the
        # invocand.

        for (my $i = 0 ; $i < $#values ; ++ $i) {
            my $fib =  $class -> bzero();
            $fib -> {value} = $values[$i];
            $values[$i] = $fib;
        }

        $x -> {value} = $values[-1];
        $values[-1] = $x;

        # If negative, insert sign as appropriate.

        if ($x -> is_neg()) {
            for (my $i = 2 ; $i <= $#values ; $i += 2) {
                $values[$i]{sign} = '-';
            }
        }

        @values = map { $_ -> round(@r) } @values;
        return @values;
    }

    # Scalar context.

    else {
        return $x if $x->modify('bdfac') || $x ->  is_inf('+');
        return $x->bnan() if $x -> is_nan() || $x -> is_inf('-');

        $x->{sign}  = $x -> is_neg() && $x -> is_even() ? '-' : '+';
        $x->{value} = $LIB->_fib($x->{value});
        return $x->round(@r);
    }
}

sub blucas {
    # compute Lucas number(s)
    my ($class, $x, @r) = objectify(1, @_);

    croak("blucas() requires a newer version of the $LIB library.")
        unless $LIB->can('_lucas');

    return $x if $x->modify('blucas');

    # List context.

    if (wantarray) {
        return () if $x -> is_nan();
        croak("blucas() can't return an infinitely long list of numbers")
            if $x -> is_inf();

        # Use the backend library to compute the first $x Lucas numbers.

        my @values = $LIB->_lucas($x->{value});

        # Make objects out of them. The last element in the array is the
        # invocand.

        for (my $i = 0 ; $i < $#values ; ++ $i) {
            my $lucas =  $class -> bzero();
            $lucas -> {value} = $values[$i];
            $values[$i] = $lucas;
        }

        $x -> {value} = $values[-1];
        $values[-1] = $x;

        # If negative, insert sign as appropriate.

        if ($x -> is_neg()) {
            for (my $i = 2 ; $i <= $#values ; $i += 2) {
                $values[$i]{sign} = '-';
            }
        }

        @values = map { $_ -> round(@r) } @values;
        return @values;
    }

    # Scalar context.

    else {
        return $x if $x ->  is_inf('+');
        return $x->bnan() if $x -> is_nan() || $x -> is_inf('-');

        $x->{sign}  = $x -> is_neg() && $x -> is_even() ? '-' : '+';
        $x->{value} = $LIB->_lucas($x->{value});
        return $x->round(@r);
    }
}

sub blsft {
    # (BINT or num_str, BINT or num_str) return BINT
    # compute x << y, base n, y >= 0

    my ($class, $x, $y, $b, @r);

    # Objectify the base only when it is defined, since an undefined base, as
    # in $x->blsft(3) or $x->blog(3, undef) means use the default base 2.

    if (!ref($_[0]) && $_[0] =~ /^[A-Za-z]|::/) {
        # E.g., Math::BigInt->blog(256, 5, 2)
        ($class, $x, $y, $b, @r) =
          defined $_[3] ? objectify(3, @_) : objectify(2, @_);
    } else {
        # E.g., Math::BigInt::blog(256, 5, 2) or $x->blog(5, 2)
        ($class, $x, $y, $b, @r) =
          defined $_[2] ? objectify(3, @_) : objectify(2, @_);
    }

    return $x if $x -> modify('blsft');
    return $x -> bnan() if ($x -> {sign} !~ /^[+-]$/ ||
                            $y -> {sign} !~ /^[+-]$/);
    return $x -> round(@r) if $y -> is_zero();

    $b = defined($b) ? $b -> numify() : 2;

    # While some of the libraries support an arbitrarily large base, not all of
    # them do, so rather than returning an incorrect result in those cases,
    # disallow bases that don't work with all libraries.

    my $uintmax = ~0;
    croak("Base is too large.") if $b > $uintmax;

    return $x -> bnan() if $b <= 0 || $y -> {sign} eq '-';

    $x -> {value} = $LIB -> _lsft($x -> {value}, $y -> {value}, $b);
    $x -> round(@r);
}

sub brsft {
    # (BINT or num_str, BINT or num_str) return BINT
    # compute x >> y, base n, y >= 0

    # set up parameters
    my ($class, $x, $y, $b, @r) = (ref($_[0]), @_);

    # objectify is costly, so avoid it
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $x, $y, $b, @r) = objectify(2, @_);
    }

    return $x if $x -> modify('brsft');
    return $x -> bnan() if ($x -> {sign} !~ /^[+-]$/ || $y -> {sign} !~ /^[+-]$/);
    return $x -> round(@r) if $y -> is_zero();
    return $x -> bzero(@r) if $x -> is_zero(); # 0 => 0

    $b = 2 if !defined $b;
    return $x -> bnan() if $b <= 0 || $y -> {sign} eq '-';

    # this only works for negative numbers when shifting in base 2
    if (($x -> {sign} eq '-') && ($b == 2)) {
        return $x -> round(@r) if $x -> is_one('-'); # -1 => -1
        if (!$y -> is_one()) {
            # although this is O(N*N) in calc (as_bin!) it is O(N) in Pari et
            # al but perhaps there is a better emulation for two's complement
            # shift...
            # if $y != 1, we must simulate it by doing:
            # convert to bin, flip all bits, shift, and be done
            $x -> binc();           # -3 => -2
            my $bin = $x -> as_bin();
            $bin =~ s/^-0b//;       # strip '-0b' prefix
            $bin =~ tr/10/01/;      # flip bits
            # now shift
            if ($y >= CORE::length($bin)) {
                $bin = '0';         # shifting to far right creates -1
                                    # 0, because later increment makes
                                    # that 1, attached '-' makes it '-1'
                                    # because -1 >> x == -1 !
            } else {
                $bin =~ s/.{$y}$//; # cut off at the right side
                $bin = '1' . $bin;  # extend left side by one dummy '1'
                $bin =~ tr/10/01/;  # flip bits back
            }
            my $res = $class -> new('0b' . $bin); # add prefix and convert back
            $res -> binc();                       # remember to increment
            $x -> {value} = $res -> {value};      # take over value
            return $x -> round(@r); # we are done now, magic, isn't?
        }

        # x < 0, n == 2, y == 1
        $x -> bdec();           # n == 2, but $y == 1: this fixes it
    }

    $x -> {value} = $LIB -> _rsft($x -> {value}, $y -> {value}, $b);
    $x -> round(@r);
}

###############################################################################
# Bitwise methods
###############################################################################

sub band {
    #(BINT or num_str, BINT or num_str) return BINT
    # compute x & y

    # set up parameters
    my ($class, $x, $y, @r) = (ref($_[0]), @_);
    # objectify is costly, so avoid it
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $x, $y, @r) = objectify(2, @_);
    }

    return $x if $x->modify('band');

    $r[3] = $y;                 # no push!

    return $x->bnan() if ($x->{sign} !~ /^[+-]$/ || $y->{sign} !~ /^[+-]$/);

    if ($x->{sign} eq '+' && $y->{sign} eq '+') {
        $x->{value} = $LIB->_and($x->{value}, $y->{value});
    } else {
        ($x->{value}, $x->{sign}) = $LIB->_sand($x->{value}, $x->{sign},
                                                $y->{value}, $y->{sign});
    }
    return $x->round(@r);
}

sub bior {
    #(BINT or num_str, BINT or num_str) return BINT
    # compute x | y

    # set up parameters
    my ($class, $x, $y, @r) = (ref($_[0]), @_);
    # objectify is costly, so avoid it
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $x, $y, @r) = objectify(2, @_);
    }

    return $x if $x->modify('bior');

    $r[3] = $y;                 # no push!

    return $x->bnan() if ($x->{sign} !~ /^[+-]$/ || $y->{sign} !~ /^[+-]$/);

    if ($x->{sign} eq '+' && $y->{sign} eq '+') {
        $x->{value} = $LIB->_or($x->{value}, $y->{value});
    } else {
        ($x->{value}, $x->{sign}) = $LIB->_sor($x->{value}, $x->{sign},
                                               $y->{value}, $y->{sign});
    }
    return $x->round(@r);
}

sub bxor {
    #(BINT or num_str, BINT or num_str) return BINT
    # compute x ^ y

    # set up parameters
    my ($class, $x, $y, @r) = (ref($_[0]), @_);
    # objectify is costly, so avoid it
    if ((!ref($_[0])) || (ref($_[0]) ne ref($_[1]))) {
        ($class, $x, $y, @r) = objectify(2, @_);
    }

    return $x if $x->modify('bxor');

    $r[3] = $y;                 # no push!

    return $x->bnan() if ($x->{sign} !~ /^[+-]$/ || $y->{sign} !~ /^[+-]$/);

    if ($x->{sign} eq '+' && $y->{sign} eq '+') {
        $x->{value} = $LIB->_xor($x->{value}, $y->{value});
    } else {
        ($x->{value}, $x->{sign}) = $LIB->_sxor($x->{value}, $x->{sign},
                                               $y->{value}, $y->{sign});
    }
    return $x->round(@r);
}

sub bnot {
    # (num_str or BINT) return BINT
    # represent ~x as twos-complement number
    # we don't need $class, so undef instead of ref($_[0]) make it slightly faster
    my ($class, $x) = ref($_[0]) ? (undef, @_) : objectify(1, @_);

    return $x if $x->modify('bnot');
    $x->binc()->bneg();         # binc already does round
}

###############################################################################
# Rounding methods
###############################################################################

sub round {
    # Round $self according to given parameters, or given second argument's
    # parameters or global defaults

    # for speed reasons, _find_round_parameters is embedded here:

    my ($self, $a, $p, $r, @args) = @_;
    # $a accuracy, if given by caller
    # $p precision, if given by caller
    # $r round_mode, if given by caller
    # @args all 'other' arguments (0 for unary, 1 for binary ops)

    my $class = ref($self);       # find out class of argument(s)
    no strict 'refs';

    # now pick $a or $p, but only if we have got "arguments"
    if (!defined $a) {
        foreach ($self, @args) {
            # take the defined one, or if both defined, the one that is smaller
            $a = $_->{_a} if (defined $_->{_a}) && (!defined $a || $_->{_a} < $a);
        }
    }
    if (!defined $p) {
        # even if $a is defined, take $p, to signal error for both defined
        foreach ($self, @args) {
            # take the defined one, or if both defined, the one that is bigger
            # -2 > -3, and 3 > 2
            $p = $_->{_p} if (defined $_->{_p}) &