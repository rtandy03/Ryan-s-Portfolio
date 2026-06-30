import streamlit as st
import pandas as pd

# Import and analyze the data
games_df = pd.read_csv("data/Games.csv")
players_df = pd.read_csv("data/Players.csv")
player_stats_df = pd.read_csv("data/PlayerStatistics.csv")
player_statsE_df = pd.read_csv("data/PlayerStatisticsExtended.csv")
team_stats_df = pd.read_csv("data/TeamStatistics.csv")
team_statsE_df = pd.read_csv("data/TeamStatisticsExtended.csv")

"""
print(f"Games cols: {games_df.columns}")
print(f"Players cols: {players_df.columns}")
print(f"Player stats cols: {player_stats_df.columns}")
print(f"Player stats extended cols: {player_statsE_df.columns}")
print(f"Team stats cols: {team_stats_df.columns}")
print(f"Team stats extended cols: {team_statsE_df.columns}")
"""

