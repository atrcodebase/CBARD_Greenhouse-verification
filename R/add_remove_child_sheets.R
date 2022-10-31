# remove the cases from child-sheets that are added mistakenly by field team ----------------------------------------
print("remove cases from child sheets")
family_roster <- family_roster %>% filter(KEY %notin% unique(remove_from_child_sheets$uuid))
crops <- crops %>% filter(KEY %notin% unique(remove_from_child_sheets$uuid))

# add cases to child-sheets ----------------------------------------
print("add new cases to child sheets")
family_roster <- rbind(family_roster, family_roster_add_to_child_sheet)
crops <- rbind(crops, crops_add_to_child_sheet)

rm(family_roster_add_to_child_sheet)
rm(crops_add_to_child_sheet)
rm(remove_from_child_sheets)
