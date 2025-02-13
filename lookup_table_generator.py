import pandas as pd
from Dictionary import goaldirectedness_dict, efficiency_dict, reciprocity_dict, ToM_dict, above_dict, cutting_dict, surrounding_dict, far_dict, falling_dict, dying_dict, growing_dict, eating_dict, besides_dict, color_dict, temperaturetime_dict, sound_dict, touch_dict, taste_dict, directions_dict, emotions_dict, injury__dict, going_dict, coming_dict, surprise_dict, deception_dict, gravity_dict, alone_dict, pulling_dict, empty_dict, night_dict, shadow_dict, carrying_dict, names_dict, smallest_dict, largest_dict, completion_dict, copy_dict, fill_dict, lower_dict, more_dict, slower_dict, containment_dict, support_dict, dry_dict, light_dict, matter_dict, proximity_dict, continuation_dict, speed_dict, age_dict, correspondence_dict, possession_dict, retrieval_dict, taste2_dict, day_dict, smallest2_dict, largest2_dict, smallest3_dict, largest3_dict, smallest4_dict, largest4_dict, smallest5_dict, lower2_dict, continuation2_dict, continuation3_dict, growing2_dict, dying2_dict, alive_dict, memory_dict, age2_dict, new_dict


def create_lookup_table(dict_list):
    records = []
    
    for d in dict_list:
        sentence_id = d['sentence_id']
        
        # Truth value prompts
        for i in range(1, 3):
            sentence_name = f"{sentence_id}_truth_value_{i}"
            coherent_answer = "True" if d[f"truth_value_answer_{i}"] else "False"
            incoherent_answer = "False" if d[f"truth_value_answer_{i}"] else "True"
            records.append({
                "sentence_name": sentence_name,
                "coherent_answer": coherent_answer,
                "incoherent_answer": incoherent_answer
            })
        
        # Continuation prompts
        for i in range(1, 4):
            question = f"{sentence_id}_continuation_{i}"
            coherent_answer = f"{d[f'continuation_prompt_{i}']} {d[f'continuation_coherent_keyword_{i}']} {d[f'continuation_coherent_suffix_{i}']}".strip()
            incoherent_answer = f"{d[f'continuation_prompt_{i}']} {d[f'continuation_incoherent_keyword_{i}']} {d[f'continuation_incoherent_suffix_{i}']}".strip()
            records.append({
                "question": question,
                "coherent_answer": coherent_answer,
                "incoherent_answer": incoherent_answer
            })
    
    df = pd.DataFrame(records)
    # add delimiter = ';'
    df.to_csv("experiment_lookup_table.csv", index=False)

# List of dictionaries (add all your dictionaries here)
dict_list = [
    goaldirectedness_dict,
    efficiency_dict,
    reciprocity_dict,
    ToM_dict,
    above_dict,
    cutting_dict,
    surrounding_dict,
    far_dict,
    falling_dict,
    dying_dict,
    growing_dict,
    eating_dict,
    besides_dict,
    color_dict,
    temperaturetime_dict,
    sound_dict,
    touch_dict,
    taste_dict,
    directions_dict,
    emotions_dict,
    injury__dict,
    going_dict,
    coming_dict,
    surprise_dict,
    deception_dict,
    gravity_dict,
    alone_dict,
    pulling_dict,
    empty_dict,
    night_dict,
    shadow_dict,
    carrying_dict,
    names_dict,
    smallest_dict,
    largest_dict,
    completion_dict,
    copy_dict,
    fill_dict,
    lower_dict,
    more_dict,
    slower_dict,
    containment_dict,
    support_dict,
    dry_dict,
    light_dict,
    matter_dict,
    proximity_dict,
    continuation_dict,
    speed_dict,
    age_dict,
    correspondence_dict,
    possession_dict,
    retrieval_dict,
    taste2_dict,
    day_dict,
    smallest2_dict,
    largest2_dict,
    smallest3_dict,
    largest3_dict,
    smallest4_dict,
    largest4_dict,
    smallest5_dict,
    lower2_dict,
    continuation2_dict,
    continuation3_dict,
    growing2_dict,
    dying2_dict,
    alive_dict,
    memory_dict,
    age2_dict,
    new_dict
]


create_lookup_table(dict_list)
