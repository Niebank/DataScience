
import warnings

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

from sklearn.datasets import fetch_openml
from sklearn.dummy import DummyRegressor
from sklearn.compose import ColumnTransformer
from sklearn.linear_model import Ridge, PoissonRegressor
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import FunctionTransformer, OneHotEncoder
from sklearn.preprocessing import OrdinalEncoder
from sklearn.preprocessing import StandardScaler, KBinsDiscretizer
from sklearn.ensemble import RandomForestRegressor
from sklearn.utils import gen_even_slices
from sklearn.metrics import auc

from sklearn.metrics import mean_squared_error, mean_absolute_error
from sklearn.metrics import mean_poisson_deviance


def load_mtpl2(n_samples=100000):


  
    df = fetch_openml(data_id=41214, as_frame=True)['data']

  
    for column_name in df.columns[df.dtypes.values == np.object]:
        df[column_name] = df[column_name].str.strip("'")
    if n_samples is not None:
        return df.iloc[:n_samples]
    return df



df = load_mtpl2(n_samples=300000)


df["Sales"] = df["Sales"].clip(upper=1)



log_scale_transformer = make_pipeline(
    FunctionTransformer(np.log, validate=False),
    StandardScaler()
)

linear_model_preprocessor = ColumnTransformer(
    [
        ("passthrough_numeric", "passthrough",
            ["Malus"]),
        ("binned_numeric", KBinsDiscretizer(n_bins=10),
            ["Age", "Driv"]),
        ("log_scaled_numeric", log_scale_transformer,
            ["Dens"]),
        ("onehot_categorical", OneHotEncoder(),
            ["Brand", "Power", "Gas", "Region", "Area"]),
    ],
    remainder="drop",
)



df["Frequency"] = df["ClaimNb"] / df["Sales"]

print(
   pd.cut(df["Frequency"], [-1e-6, 1e-6, 1, 2, 3, 4, 5]).value_counts()
)

print("Average Frequency = {}"
      .format(np.average(df["Frequency"], weights=df["Sales"])))

print("Percentage of zero claims = {0:%}"
      .format(df.loc[df["ClaimNb"] == 0, "Sales"].sum() /
              df["Sales"].sum()))


df_train, df_test = train_test_split(df, random_state=0)

dummy = make_pipeline(
    linear_model_preprocessor,
    DummyRegressor(strategy='mean')
)
dummy.fit(df_train, df_train["Frequency"],
          dummyregressor__sample_weight=df_train["Sales"])


def score_estimator(estimator, df_test):
    """Score an estimator on the test set."""

    y_pred = estimator.predict(df_test)

    print("MSE: %.3f" %
          mean_squared_error(df_test["Frequency"], y_pred,
                             df_test["Sales"]))
    print("MAE: %.3f" %
          mean_absolute_error(df_test["Frequency"], y_pred,
                              df_test["Sales"]))

    # ignore non-positive predictions, as they are invalid for
    # the Poisson deviance
    mask = y_pred > 0
    if (~mask).any():
        warnings.warn("Estimator yields non-positive predictions for {} "
                      "samples out of {}. These will be ignored while "
                      "computing the Poisson deviance"
                      .format((~mask).sum(), mask.shape[0]))

    print("mean Poisson deviance: %.3f" %
          mean_poisson_deviance(df_test["Frequency"][mask],
                                y_pred[mask],
                                df_test["Sales"][mask]))


print("Constant mean frequency evaluation:")
score_estimator(dummy, df_test)



ridge = make_pipeline(linear_model_preprocessor, Ridge(alpha=1.0))
ridge.fit(df_train, df_train["Frequency"],
          ridge__sample_weight=df_train["Sales"])



print("Ridge evaluation:")
score_estimator(ridge, df_test)


poisson = make_pipeline(
    linear_model_preprocessor,
    PoissonRegressor(alpha=1/df_train.shape[0], max_iter=1000)
)
poisson.fit(df_train, df_train["Frequency"],
            poissonregressor__sample_weight=df_train["Sales"])

print("PoissonRegressor evaluation:")
score_estimator(poisson, df_test)



rf_preprocessor = ColumnTransformer(
    [
        ("categorical", OrdinalEncoder(),
            ["Brand", "Power", "Gas", "Region", "Area"]),
        ("numeric", "passthrough",
            ["Veh", "Driv", "Malus", "Den"]),
    ],
    remainder="drop",
)
rf = make_pipeline(
    rf_preprocessor,
    RandomForestRegressor(min_weight_fraction_leaf=0.01, n_jobs=2)
)
rf.fit(df_train, df_train["Frequency"].values,
       randomforestregressor__sample_weight=df_train["Sales"].values)


print("RandomForestRegressor evaluation:")
score_estimator(rf, df_test)



fig, axes = plt.subplots(2, 4, figsize=(16, 6), sharey=True)
fig.subplots_adjust(bottom=0.2)
n_bins = 20
for row_idx, label, df in zip(range(2),
                              ["train", "test"],
                              [df_train, df_test]):
    df["Frequency"].hist(bins=np.linspace(-1, 30, n_bins),
                         ax=axes[row_idx, 0])

    axes[row_idx, 0].set_title("Data")
    axes[row_idx, 0].set_yscale('log')
    axes[row_idx, 0].set_xlabel("y (observed Frequency)")
    axes[row_idx, 0].set_ylim([1e1, 5e5])
    axes[row_idx, 0].set_ylabel(label + " samples")

    for idx, model in enumerate([ridge, poisson, rf]):
        y_pred = model.predict(df)

        pd.Series(y_pred).hist(bins=np.linspace(-1, 4, n_bins),
                               ax=axes[row_idx, idx+1])
        axes[row_idx, idx + 1].set(
            title=model[-1].__class__.__name__,
            yscale='log',
            xlabel="y_pred (predicted expected Frequency)"
        )
plt.tight_layout()



def _mean_frequency_by_risk_group(y_true, y_pred, sample_weight=None,
                                  n_bins=100):

    idx_sort = np.argsort(y_pred)
    bin_centers = np.arange(0, 1, 1/n_bins) + 0.5/n_bins
    y_pred_bin = np.zeros(n_bins)
    y_true_bin = np.zeros(n_bins)

    for n, sl in enumerate(gen_even_slices(len(y_true), n_bins)):
        weights = sample_weight[idx_sort][sl]
        y_pred_bin[n] = np.average(
            y_pred[idx_sort][sl], weights=weights
        )
        y_true_bin[n] = np.average(
            y_true[idx_sort][sl],
            weights=weights
        )
    return bin_centers, y_true_bin, y_pred_bin

fig, ax = plt.subplots(nrows=1, ncols=3, figsize=(12, 3.5))
plt.subplots_adjust(wspace=0.3)

for axi, model in zip(ax, [ridge, poisson, rf]):
    y_pred = model.predict(df_test)

    q, y_true_seg, y_pred_seg = _mean_frequency_by_risk_group(
        df_test["Frequency"].values,
        y_pred,
        sample_weight=df_test["Sales"].values,
        n_bins=10)

    axi.plot(q, y_pred_seg, marker='o', linestyle="-", label="predictions")
    axi.plot(q, y_true_seg, marker='x', linestyle="--", label="observations")
    axi.set_xlim(0, 1.0)
    axi.set_ylim(0, 0.6)
    axi.set(
        title=model[-1].__class__.__name__,
        xlabel='Fraction of samples sorted by y_pred',
        ylabel='Mean Frequency (y_pred)'
    )
    axi.legend()
plt.tight_layout()

def _cumulated_claims(y_true, y_pred, exposure):
    idx_sort = np.argsort(y_pred)  # from safest to riskiest
    sorted_exposure = exposure[idx_sort]
    sorted_frequencies = y_true[idx_sort]
    cumulated_exposure = np.cumsum(sorted_exposure)
    cumulated_exposure /= cumulated_exposure[-1]
    cumulated_claims = np.cumsum(sorted_exposure * sorted_frequencies)
    cumulated_claims /= cumulated_claims[-1]
    return cumulated_exposure, cumulated_claims

fig, ax = plt.subplots(figsize=(8, 8))

for model in [ridge, poisson, rf]:
    y_pred = model.predict(df_test)
    cum_exposure, cum_claims = _cumulated_claims(
        df_test["Frequency"].values,
        y_pred,
        df_test["Sales"].values)
    area = auc(cum_exposure, cum_claims)
    label = "{} (area under curve: {:.3f})".format(
        model[-1].__class__.__name__, area)
    ax.plot(cum_exposure, cum_claims, linestyle="-", label=label)

# Oracle model: y_pred == y_test
cum_exposure, cum_claims = _cumulated_claims(
    df_test["Frequency"].values,
    df_test["Frequency"].values,
    df_test["Sales"].values)
area = auc(cum_exposure, cum_claims)
label = "Oracle (area under curve: {:.3f})".format(area)
ax.plot(cum_exposure, cum_claims, linestyle="-.", color="gray", label=label)

# Random Baseline
ax.plot([0, 1], [0, 1], linestyle="--", color="black",
        label="Random baseline")
ax.set(
    title="Cumulated number of images by model",
    xlabel='Fraction of exposure (from safest to riskiest)',
    ylabel='Fraction of number of images'
)
ax.legend(loc="upper left")

plt.show()
