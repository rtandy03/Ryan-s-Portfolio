import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

def main():
    ai_df = pd.read_csv("data/AI_data.csv")
    sgy_df = pd.read_csv("data/school_data.csv")

    # print(merged_df.columns)
    # print(min(merged_df["Mn_Avg_ol"]))
    # print(max(merged_df["Mn_Avg_ol"]))

    # print(merged_df)
    groups = set(sgy_df["Subgroup_Case"])
    print(groups)

    grouped = {"hsp": "Hispanic", "mal": "Male", "blk": "Black", "all": "Total", "fem": "Female",
                "wht": "White", "asn": "Asian"}

    ### Plot AI vs education by subgroup
    for curr in groups:
        if curr in grouped:
            plot_ai_educ_by_subgroup(ai_df, sgy_df, curr, grouped)

    ### Find correlation between AI and avg test scores
    corr = find_correlation(ai_df, sgy_df)
    print(f"Correlation: {corr}")

    ### Create Scattorer plot of AI vs education
    scatter_ai_educ(ai_df, sgy_df)

    ### Compute R square
    r_square = calc_r_square(corr)
    print(f"R^2: {r_square}")


# Plots AI adoption rate vs different subgroup case avg mean test score
def plot_ai_educ_by_subgroup(ai_df, sgy_df, category, grouped):
    subgroup_df = sgy_df.groupby(["Year", "Subgroup_Case"])["Mn_Avg_ol"].mean().reset_index()
    subgroup_df = subgroup_df[subgroup_df["Year"] > 2017]
    ai_rev_df = ai_df[["Year", "AI_SW_Rev(Billions)", "Globe_AI_Market(Billions)", "AI_Adoption"]]
    ai_rev_df.loc[:, "AI_Adoption"] = ai_rev_df["AI_Adoption"].apply(lambda x: float(x.strip('%')) / 100)
    
    df = pd.merge(left=subgroup_df, right=ai_rev_df, how="outer", on="Year").dropna()

    fig, ax = plt.subplots()

    sns.lineplot(data=df, x="Year", y="AI_Adoption", ax=ax, color="blue", label="AI Adoption")
    sns.lineplot(data=df[df["Subgroup_Case"] == category], x="Year", y="Mn_Avg_ol", color="green", ax=ax, label=f"{grouped.get(category)} avg test scores")

    sns.set_theme(style="darkgrid")
    plt.ylim(-.8, .6)
    plt.xlabel("Year")
    plt.ylabel("Value")
    plt.title(f"AI Adoption vs {grouped.get(category)} test score")
    plt.legend(loc="upper left")
    plt.show()


# Returns correlation between the mean average grade per year and the AI adoption rate
def find_correlation(ai_df, sgy_df):
    sgy_year_df = sgy_df[sgy_df["Subgroup_Case"] == "all"].groupby(["Year"])["Mn_Avg_ol"].mean().reset_index().dropna()
    # print(sgy_year_df)
    years_to_keep = [2018, 2019, 2022, 2023, 2024]
    sgy_year_df = sgy_year_df[sgy_year_df["Year"].isin(years_to_keep)] 
    ai_df.loc[:, "AI_Adoption"] = ai_df["AI_Adoption"].apply(lambda x: float(x.strip('%')) / 100)
    ai_df = ai_df[ai_df["Year"].isin(years_to_keep)]

    # print(ai_df)
    # print(sgy_year_df)

    adopt = list(ai_df["AI_Adoption"])
    grades = list(sgy_year_df["Mn_Avg_ol"])

    ### Calculate Covariance
    
    # Calculate Sxy
    Sxy = 0
    xbar = sum(adopt) / len(adopt)
    ybar = sum(grades) / len(grades)

    for i in range(len(adopt)):
        Sxy += ((adopt[i] - xbar) * (grades[i] - ybar))
    covar = Sxy / (len(adopt) - 1)

    ### Calculate & Return Correlation
    return covar / (ai_df["AI_Adoption"].std() * sgy_year_df["Mn_Avg_ol"].std())


### Calculates R^2 -> How much of the variation in the dependent var explained by the regression model
# goodness of the fit
def calc_r_square(corr):
    return corr**2


### Scatter plot between AI adoption and mean test scores
def scatter_ai_educ(ai_df, sgy_df):
    sgy_year_df = sgy_df[sgy_df["Subgroup_Case"] == "all"].groupby(["Year"])["Mn_Avg_ol"].mean().reset_index().dropna()
    years_to_keep = [2018, 2019, 2022, 2023, 2024]
    sgy_year_df = sgy_year_df[sgy_year_df["Year"].isin(years_to_keep)] 
    ai_df.loc[:, "AI_Adoption"] = ai_df["AI_Adoption"].apply(
        lambda x: float(str(x).strip('%')) / 100 if isinstance(x, str) else x
    )
    ai_df = ai_df[ai_df["Year"].isin(years_to_keep)]

    comb_df = pd.merge(ai_df, sgy_year_df, on="Year")
    comb_df["AI_Adoption"] = pd.to_numeric(comb_df["AI_Adoption"], errors="coerce")
    comb_df["Mn_Avg_ol"] = pd.to_numeric(comb_df["Mn_Avg_ol"], errors="coerce")
    
    sns.regplot(data=comb_df, x='AI_Adoption', y="Mn_Avg_ol", scatter=True, ci=None, line_kws={"linestyle": "--", "color": "red"})

    plt.title("Mean test score vs AI adoption")
    plt.xlabel("AI adoption")
    plt.ylabel("Mean test score")

    plt.xlim(-.1, .6)
    plt.ylim(-.4, .4)
    plt.show()


main()