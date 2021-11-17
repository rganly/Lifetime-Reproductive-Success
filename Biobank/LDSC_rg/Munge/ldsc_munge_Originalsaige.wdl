
task munge_fg {

    String pheno
    File sumstats
    File rsfile
    Int n_cases
    Int n_controls
    String docker

    command <<<

        gunzip -c ${sumstats} | \
        awk 'BEGIN{FS=OFS="\t"} \
        NR==1{for(i=1;i<=NF;i++) a[$i]=i; print "SNP","A1","A2","BETA","P"} \
        NR>1 {print $a["#chrom"]"_"$a["pos"],$a["ref"],$a["alt"],$a["beta"],$a["pval"]}' | \
        awk 'BEGIN{FS=OFS="\t"} {n=split($1,a,","); for(i=1;i<=n;i++) print a[i],$0}' | \
        cut -f1,3- > ${pheno}.temp.premunge
        cat ${pheno}.temp.premunge|head
        
        cat ${rsfile}|grep -v SNP|sort -k1,1|head
        sort -k1,1 ${pheno}.temp.premunge|head
        
        echo "SNP A1 A2 BETA P" >> ${pheno}.temp2.premunge
        join -1 1 -2 1 <(cat ${rsfile}|grep -v SNP|sort -k1,1) <(sort -k1,1 ${pheno}.temp.premunge) |awk '{print $2,$3,$4,$5,$6}' >> ${pheno}.temp2.premunge
        cat ${pheno}.temp2.premunge| tr ' ' '\t'|gzip > ${pheno}.premunge.gz

        cat ${pheno}.premunge.gz|zcat|wc -l
        cat ${pheno}.premunge.gz|zcat|head
        
        munge_sumstats.py \
        --sumstats ${pheno}.premunge.gz \
        --N-cas ${n_cases} \
        --N-con ${n_controls} \
        --out ${pheno}.ldsc \
        --merge-alleles /w_hm3.snplist

    >>>


    output {
        File out = pheno + ".ldsc.sumstats.gz"
        File log = pheno + ".ldsc.log"
    }

    runtime {
        docker: "${docker}"
        cpu: 1
        memory: "20 GB"
        disks: "local-disk 200 HDD"
        zones: "europe-west1-b"
        preemptible: 2
        noAddress: true
    }
}



workflow ldsc_munge {

    File sumstats_lst
    String docker

    Array[Array[String]] sumstats = read_tsv(sumstats_lst)

    scatter (stats in sumstats) {
        call munge_fg {
            input: docker=docker, pheno=stats[0], sumstats=stats[1], n_cases=stats[2], n_controls=stats[3], rsfile=stats[4]
        }
    }

}


