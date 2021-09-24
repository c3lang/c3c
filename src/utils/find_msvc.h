#pragma once

typedef struct {
    char first[260];
    char second[260];
} PathPair;

PathPair get_latest_available_vs_path();
PathPair find_winkit_path();