"""Utility functions

Functions used in jupyter notebooks to load, reshape, format, and conduct
tests for statistical significance.

"""
import json
import itertools
import numpy as np
import pandas as pd
import scipy.stats as st


# ==============================================================================
# Data Handling

def read_json(fp):
    """Read in a json file (not lines)"""
    with open(fp, 'r') as infile:
        return json.load(infile)


def load_tsv(fp, head=False, **kwargs):
    """Shortcut to pd.read_csv(..., sep='\t') and displaying details"""
    df = pd.read_csv(fp, sep='\t', **kwargs)
    print(f"loaded: {fp} - {dfshape(df)}")
    if head: dfhead(df)
    return df


def dfshape(df):
    """Returns more readable dataframe shape numbers"""
    nrows, ncols = [f"{s:,}" for s in df.shape]
    return f"({nrows}, {ncols})"


def dfhead(df, n=3):
    display(df.head(n))
    print()


def get_nonzero_min(col):
    """Get nonzero minimum value from pd.Series"""
    return col[col > 0].min()


def mm_to_inches(mm):
    """Convert millimeters to inches"""
    cm_to_in = 1 / 2.54       # centimeters to inches ratio
    mm_to_in = cm_to_in / 10  # millimeters to inches ratio
    return mm * mm_to_in  


# ==============================================================================
# Formatting

def stringify(series, fmt=",.1f"):
    """Convert pd.Series of floats to rounded and readable strings"""
    series_fmt = series.apply(lambda i: f"{i:{fmt}}")
    nan_string = "nan" if not series_fmt.str.contains("%").any() else "nan%"
    return series_fmt.replace(nan_string, '-')


def print_line(word='', length=80):
    """Print a word followed by a line to a specified length (default=80)"""
    if word: print(word + ' ' + ''.join(['-'] * (80 - len(word))))
    else:    print(''.join(['-']*length))


# ==============================================================================
# Plot Adjustments

def remove_yaxis_ticks(ax, major=True, minor=True):
    if major:
        for tic in ax.yaxis.get_major_ticks():
            tic.tick1line.set_visible(False)
            tic.tick2line.set_visible(False)
    if minor:
        for tic in ax.yaxis.get_minor_ticks():
            tic.tick1line.set_visible(False)
            tic.tick2line.set_visible(False)


def remove_xaxis_ticks(ax, major=True, minor=True):
    if major:
        for tic in ax.xaxis.get_major_ticks():
            tic.tick1line.set_visible(False)
            tic.tick2line.set_visible(False)
    if minor:
        for tic in ax.xaxis.get_minor_ticks():
            tic.tick1line.set_visible(False)
            tic.tick2line.set_visible(False)


def reorder_legend(handles=None, labels=None, order=None, unique=False):
    """Reorder legend handles and labels with ordered list

    Credit: @CPBL
    https://stackoverflow.com/questions/22263807/how-is-order-of-items-in-matplotlib-legend-determined/35926913#35926913

    Args:
        handles (list): handles obtained via ax.get_legend_handles_labels()
        labels (list): labels obtained via ax.get_legend_handles_labels()
        order (list): list of labels in desired order, strings must match
        unique (bool): option to drop duplicates and keep first label instance

    Returns:
        tuple: the sorted handles and labels objects
    """

    # Sort both labels and handles by labels
    labels, handles = zip(*sorted(zip(labels, handles), key=lambda l: l[0]))
    if order is not None:
        # Sort according to a given list (not necessarily complete)
        keys = dict(zip(order,range(len(order))))
        labels, handles = zip(*sorted(zip(labels, handles), key=lambda l, 
                                      keys=keys: keys.get(l[0], np.inf)))

    # Keep only the first of each handle
    if unique: 
        labels, handles= zip(*unique_everseen(zip(labels,handles), key=labels))
    return handles, labels


def unique_everseen(seq, key=None):
    seen = set()
    seen_add = seen.add
    return [x for x,k in zip(seq,key) if not (k in seen or seen_add(k))]


# ==============================================================================
# Statistics

def describe(series):
    """Enhanced describe, featuring sem, median, sum, and null count
    
    Arguments:
        series {pd.Series} -- A pandas series to describe
    
    Returns:
        pd.DataFrame -- A dataframe with descriptive stats
    """
    keep_cols = ['count', 'mean', 'std', 'min', 'max']
    tab = series.describe(datetime_is_numeric=True).T
    tab = tab.reindex(keep_cols)
    
    try:
        tab['sem'] = series.sem()
        tab['median'] = series.median()
        tab['sum'] = series.sum()
        tab['num_null'] = series.isnull().sum()
        return tab
    except TypeError:
        return tab


def cumulative_sum_table(col):
    """Get a cumulative sum table from a dataframe column

    Args:
        col (pd.Series): a column of values

    Returns:
        pd.DataFrame: dataframe containing cumulative sum of values and index
    """
    col = col.sort_values(ascending=False)
    df = col.cumsum().reset_index()
    df['n'] = range(1, col.shape[0] + 1)
    df.drop('index', axis=1, inplace=True)
    df.columns = ['n_cumsum', 'n']
    df['p'] = df['n'] / col.shape[0]
    df['p_cumsum'] = df['n_cumsum'] / col.sum()
    
    return df


def kruskal_test(data, group, metric, fmt=False, nan_policy='omit'):
    """Formatted Kruskal-Wallis H test results"""
    groups = {key:value for key, value in data.groupby(group)[metric]}
    H, P = st.kruskal(*groups.values(), nan_policy=nan_policy)
    output = {'H':H, 'P':P} if not fmt else (f'{H:.3f}', f'{P:.4f}')
    return output


def kruskal_table(data, groups, metrics):
    """Multiple Kruskal-Wallis H test results"""
    out_list = []
    for group, metric in itertools.product(groups, metrics):
        result = kruskal_test(data, group=group, metric=metric)
        out_list.append({
            'group': group, 
            'metric': metric,
            'n': (data[group].notnull() & data[metric].notnull()).sum(),
            'H': result['H'],
            'P': result['P'],
        })  
    
    # Convert to dataframe and format floats
    tab = pd.DataFrame(out_list)
    tab['H'] = stringify(tab['H'], fmt=".2f")
    tab['P'] = stringify(tab['P'], fmt=".3f") + tab['P'].apply(p_value_sig)
    return tab


def spearmanr(data, metric1, metric2, fmt=False, nan_policy='omit'):
    """Formatted Spearman's rank correlation coefficient test results"""
    rho, p = st.spearmanr(data[metric1], data[metric2], nan_policy=nan_policy)
    output = {'rho':rho, 'p':p} if not fmt else (f'{rho:.3f}', f'{p:.4f}')
    return output

def p_value_sig(p_value, 
                p_cutoffs=[0.001, 0.01, 0.05],
                p_symbols=['***', '**', '*']):
    """Generate P-value significance markers"""
    
    for p_cutoff, p_symbol in zip(p_cutoffs, p_symbols):
        if p_value < p_cutoff:
            return p_symbol
    return ""
