#include <Rcpp.h>
using namespace Rcpp;

// #define CHECK_DEBUG
// #define SIMPLE_DEBUG


// [[Rcpp::export]]
bool is_cell_empty(Nullable<CharacterVector> roomData_){
  if(roomData_.isNull()) return true;
  CharacterVector roomData(roomData_);
  // Rcout << "\nCell Size: " << roomData.size() << "\t" << (std::string) collapse(roomData);
  if(roomData.size() <= 0) return true;
  if(roomData[0] == "") return true;
  return false;
}

bool check_sets_cpp(Nullable<List> sets_ = R_NilValue, CharacterVector unique_ids = CharacterVector(), bool neg=false){
  if(sets_.isNull()){
    return true;
  }
  Rcpp::List sets(sets_);
  if(sets.size() <= 0){
    return true;
  }
  for(int s = 0; s < sets.size(); s ++){
    CharacterVector set = as<CharacterVector>(sets[s]);
#ifdef SIMPLE_DEBUG
    Rcout << "\tSets: " <<  (std::string) collapse(set);
#endif
    if(set.size() <= 0) continue;
    // if(set.size() > unique_ids.size()) continue; // it does not matter whether the set is larger or smaller, only ONE of the set needs to be there
#ifdef CHECK_DEBUG
    std::string set_str = collapse(set);
    std::string ids_str = collapse(unique_ids);
    Rcout << "\nChecking sets: " << set_str << " in " << ids_str << ": ";
#endif
    for(int i = 0; i < set.size(); i++){
      for(int j = 0; j < unique_ids.size(); j++){
        if(set[i] == unique_ids[j]){
#ifdef CHECK_DEBUG
          Rcout << true;
          Rcout << "\n\tSet found " << set[i] << " != " << unique_ids[j];
#endif
          return true;
        }
      }
    }
  }
#ifdef CHECK_DEBUG
  Rcout << false;
#endif
  return false;
}

bool check_range_cpp(Nullable<NumericVector> range_ = R_NilValue, CharacterVector unique_ids = CharacterVector(), bool neg=false){
  // if(unique_ids.size() <= 0){
  //   return false;
  // }
  if(range_.isNull()){
    return true;
  }
  NumericVector range;
  range = range_;
  if(range.size() <= 0){
    return true;
  }

  int min = range[0];
  int max = range.size() > 1 ? range[1] : R_PosInf;
#ifdef SIMPLE_DEBUG
  Rcout << "\tRange: " <<  "[" << min << ", " << max << "]: ";
#endif
  bool in_range = unique_ids.size() >= min && unique_ids.size() <= max;
#ifdef CHECK_DEBUG
  Rcout << "\nChecking range: " << unique_ids.size() << " in " << "[" << min << ", " << max << "]: " << in_range;
#endif
  return in_range;
}

bool check_groups_cpp(Nullable<List> groups_ = R_NilValue, CharacterVector unique_ids = CharacterVector(), bool neg=false){
  if(groups_.isNull()){
    return true;
  }

  Rcpp::List groups(groups_);
  if(groups.size() <= 0){
    return true;
  }

  int groups_found = 0;
  for(int g = 0; g < groups.size(); g++){
    CharacterVector group = as<CharacterVector>(groups[g]);
#ifdef SIMPLE_DEBUG
    Rcout << "\tGroups: " <<  (std::string) collapse(group);
#endif
#ifdef CHECK_DEBUG
    std::ostringstream group_str;
    std::ostringstream ids_str;
    int last_added = -1;
#endif
    if(group.size() <= 0) return true;
    if(group.size() > unique_ids.size()) return false; // Because in the group ALL of the agents have to be there, if the group is larger than the amount of agents in the room, it can't happen
    int found = 0;
    for(int i = 0; i < group.size(); i++){
#ifdef CHECK_DEBUG
      group_str << group[i] << (i >= group.size()-1 ? "" :", ");
#endif
      for(int j = 0; j < unique_ids.size(); j++){
#ifdef CHECK_DEBUG
        if(j > last_added){
          ids_str << unique_ids[j] << (j >= unique_ids.size()-1 ? "" :", ");
          last_added = j;
        }
#endif
        if(group[i] == unique_ids[j]){
          found += 1;
          break;
        }
      }
      if(found >= group.size()){
        groups_found += 1;
        break;
      }
    }
#ifdef CHECK_DEBUG
    Rcout << "\nChecking group: [" << group_str.str() << "] in cell [" << ids_str.str() << "]: ";
    Rcout << (found >= group.size() ? "true" : "false");
    Rcout << "\tGroups Found: " << groups_found << "/" << groups.size();
#endif
    if(groups_found >= groups.size()){
#ifdef CHECK_DEBUG
      Rcout << "\tAll groups Found!";
#endif
      return true;
    }
    else{
#ifdef CHECK_DEBUG
      // Rcout << "\tGroups MISSING!";
#endif
    }
  }
  return false;
}

// [[Rcpp::export]]
bool check_cell_cpp(Nullable<CharacterVector> roomData_, Nullable<List> sets_ = R_NilValue, Nullable<List> groups_ = R_NilValue , Nullable<NumericVector> range_ = R_NilValue) {
  if(roomData_.isNull()){
    if(range_.isNull()) return false; // If the cell is empty we should return false as no set or range is in the cell.
    return check_range_cpp(range_, CharacterVector::create());
  }
  CharacterVector roomData(roomData_);
  CharacterVector unique_ids = unique(roomData);
#ifdef SIMPLE_DEBUG
  Rcout << "\nChecking Room: " <<  (std::string) collapse(unique_ids);
#endif
  bool in_range = check_range_cpp(range_, unique_ids);
  if(!in_range) return false;
  if(unique_ids.size() <= 0) return false;
  bool at_least_one = check_sets_cpp(sets_, unique_ids);
  if(!at_least_one) return false;
  bool all_in_room = check_groups_cpp(groups_, unique_ids);
  if(!all_in_room) return false;
#ifdef SIMPLE_DEBUG
  Rcout << "\t TRUE";
#endif
  return true;
}
