module DictArraysExt

using DictArrays
using DictArrays.Dictionaries
import FixedWidthTables: _colspecs_from_pairs

_colspecs_from_pairs(::Type{DictArray}, pairs) = dictionary(pairs)

end
