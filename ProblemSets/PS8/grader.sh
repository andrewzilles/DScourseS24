# array of user names
names=("phenomenon0" "reblcro" "menyangdavila" "collin-devore" "dunk0002" "whentostart" "Cfinley7" "OpalIrene" "anagallart" "TrevorRJones" "onishisaryu" "anaisouedraogo" "StevenPlai" "bradley-rann" "coltonsteele1999" "bravehaochenggo")

cd ~/teaching/stud-repo-DS24

# Loop through the array using an integer counter
for (( i=0; i<${#names[@]}; i++ )); do
    u=${names[$i]}
    echo ""
    echo ""
    echo "---------------------------------------------------"
    echo ""
    echo ""
    #echo "user $(($i+1))"
    echo "user $u"
    # 1 -- free points
    points=5
    cd ${u}/DScourseS24/ProblemSets/PS8
    
    # 2 -- is fork synced? (can't check this here)
    forkstatus=$(check_github_fork "https://github.com/${u}/DScourseS24")
    echo "${forkstatus}" 
    if echo "${forkstatus}" | grep -q -E "the fork is ahead|2 commits behind|1 commit behind"; then
        points=$((points + 5))
    fi
    
    # 4 -- PS8_*.{R,py,jl,ipynb} file
    for file in PS8_*.{r,R,py,jl,ipynb}; do
        if [[ -f $file ]]; then
            points=$((points + 35))
            echo "$file exists"
            cat $file
        else
            echo "No PS8_* script file"
        fi
    done

    # 5 -- .tex and .pdf file in PS8 folder
    for file in PS8_*.{tex,pdf,txt}; do
        ext="${file: -3}"
        if [[ -f $file ]]; then
            echo "$file exists"
            if [[ $ext != "pdf" ]]; then
                cat $file
            fi
            if [[ $ext == "pdf" ]]; then
                points=$((points + 5))
            fi
        else
            if [[ $ext == "pdf" ]]; then
                echo "No PS8_*.pdf writeup file"
            fi
            if [[ $ext != "pdf" ]]; then
                echo "No PS8_*.tex writeup file"
            fi
        fi
    done

    echo " "
    echo " "
    echo "Total points: $points"
    echo " "
    cd ../../../..
done

cd ~/teaching/DScourseS24/ProblemSets/PS8

