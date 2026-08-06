import types
import warnings

warnings.simplefilter(action="ignore", category=FutureWarning)
warnings.simplefilter(action="ignore", category=DeprecationWarning)
warnings.filterwarnings("ignore")

# The bilingual chapters are rendered with the knitr engine, so Python cells run
# through reticulate. Reticulate only turns a returned object into an HTML table
# if `_repr_html_` is a *bound method* (it checks `PyMethod_Check`). pyfixest
# attaches `_repr_html_` as a plain lambda to the great_tables instance it
# returns, which fails that check, so `pf.etable()` degrades to the object's
# repr. Re-binding the override as a real method restores the table.
import pyfixest as _pyfixest


def _rebind_repr_html(obj):
    override = getattr(obj, "__dict__", {}).get("_repr_html_")
    if override is not None and not isinstance(override, types.MethodType):
        html = override()
        obj._repr_html_ = types.MethodType(lambda self: html, obj)
    return obj


if not getattr(_pyfixest.etable, "_tidy_finance_patched", False):
    _pyfixest_etable = _pyfixest.etable

    def _etable(*args, **kwargs):
        return _rebind_repr_html(_pyfixest_etable(*args, **kwargs))

    _etable._tidy_finance_patched = True
    _pyfixest.etable = _etable
