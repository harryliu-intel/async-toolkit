#ifndef __BD_HIST_MON_CC__
#define __BD_HIST_MON_CC__

#include <stdint.h>
#include <stdio.h>
#include <string>
#include <cstring>
#include <fstream>
#include <iostream>
//#include "HistoryMonitor.h"

using namespace std;

//static HistoryMonitor * hist_mon = NULL;
static int node_count = 0;
static bool sim_done = false;
static ofstream hist_file;

extern "C" void histmon_register_node(const char* node_name) {
    //if (hist_mon == NULL) {
    //    hist_mon = new HistoryMonitor();
    //}
    //hist_mon->RegisterNode(node_name);
    if (node_count == 0) {
      hist_file.open("bd_hist.log", std::ios::app);
    }
    node_count++;
    cout << "Count=" << node_count << " Name=" << node_name << endl;
    //return 1;
}

extern "C" void histmon_add_transition(const char* node_name, int64_t time1, const char* trigger_name, int64_t time2, const char* monitor_name) {
    //hist_mon->AddTransition(node_name, time1);
    string clean_node_name(node_name);
    clean_node_name = clean_node_name.erase(clean_node_name.find("mon"), strlen("mon")+1);
    string clean_trig_name(trigger_name);
    clean_trig_name = clean_trig_name.erase(clean_trig_name.find("mon"), strlen("mon")+1);

    hist_file << clean_node_name << "@" << time1 << ":" << clean_trig_name << "@" << time2 << endl;
}

extern "C" void histmon_sim_done() {
    if (!sim_done) {
        cout << "Simulation done" << endl;
        hist_file.close(); // Close the file
        //hist_mon->PrintCppVer();
        //hist_mon->PrintStats();
        sim_done = true;
    }
}

#endif  //__BD_HIST_MON_CC__
