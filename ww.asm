assume cs:code,ds:data,ss:stack

data segment
    choice db 'Enter a number to select a function',0dh,0ah,'$'
    menu   db '1. Input information',0dh,0ah
           db '2. Search by id',0dh,0ah
           db '3. Search by name',0dh,0ah
           db '4. Sort by grades',0dh,0ah
           db '5. Show score range',0dh,0ah
           db '6. Exit',0dh,0ah,'$'
    
    welcome db 'Welcome to Grade Management System',0dh,0ah,'$'
    how_much_student db 'The number of students(1-20): ','$'
    ;学生信息输入提示    
    input_name    db 0dh,0ah,'Enter name (up to 5 chars,press Enter to finish): $'
    input_id      db 0dh,0ah,'Enter ID (up to 8 chars,press Enter to finish): $'
    input_scores  db 0dh,0ah,'Enter 8 assignment scores (0-100,separate with space): $'
    input_bw      db 0dh,0ah,'Enter big assignment score (bw,0-100): $'
    
    ;错误提示信息
    error0  db 0dh,0ah,'Invalid input! Please try again.',0dh,0ah,'$'
    error_range db 0dh,0ah,'Score must be between 0-100!',0dh,0ah,'$'
    not_found_msg db 0dh,0ah,'Student not found!',0dh,0ah,'$'
    no_student_msg db 0dh,0ah,'No student data available!',0dh,0ah,'$'
    
    ;数据显示标题
    student_header db 0dh,0ah,'Name     ID      S1 S2 S3 S4 S5 S6 S7 S8  BW  FI  Rank',0dh,0ah,'$'
    score_range_header db 0dh,0ah,'Score Range   Count',0dh,0ah,'$'
    range_90_100  db '90-100:     $'
    range_80_89   db '80-89:      $'
    range_70_79   db '70-79:      $'
    range_60_69   db '60-69:      $'
    range_0_59    db '0-59:       $'
    
    ;统计信息标题
    highest_msg db 0dh,0ah,'Highest score: $'
    lowest_msg  db 0dh,0ah,'Lowest score: $'
    average_msg db 0dh,0ah,'Average score: $'
    
    ;统计数据变量
    highest_score db 0    ;最高分
    lowest_score  db 0    ;最低分
    average_score db 0    ;平均分
    
    max_students equ 20      ;最大学生数量增加到20
    student_size equ 24      ;每个学生24字节
    
    ;学生数据结构：5字节姓名 + 8字节ID + 8门成绩(8字节) + bw(1)+fi(1)+rank(1) = 24字节
    student_info db max_students * student_size dup(0)
                 
    ;变量
    current_count db 0        ;当前学生数量
    search_buffer db 10 dup(0) ;搜索缓冲区
    
    ;分数段统计结构 - 只保留计数
    score_ranges db 5 dup(0)    ;计数
data ends

;堆栈段定义
stack segment stack 
    db 256 dup (0)  ;256字节堆栈空间
stack ends 

;----------------宏定义-------------

;实现打印字符串功能
output macro str
    push dx
    push ax
    lea dx,str    ;加载字符串地址
    mov ah,09h
    int 21h
    pop ax
    pop dx
endm

;保存寄存器
push_all macro
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push bp
endm

;恢复寄存器
pop_all macro
    pop bp
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
endm
;打印空格
print_space macro
	push dx
    push ax
    mov dl,' '
    mov ah,02h
    int 21h
    pop ax
    pop dx
endm

code segment
start:
    mov ax,data
    mov ds,ax
    mov es,ax      ;设置ES=DS用于字符串操作
    
    output welcome
    mov current_count,0  ;初始化学生数量为0
    
    ;初始化统计值
    mov highest_score,0
    mov lowest_score,0
    mov average_score,0
    
    ;跳转表放在代码段中
    jmp loopS
      ;跳转表  
func_table:
    dw input         ;功能1:输入学生信息
    dw SerByID    ;功能2:按学号搜索
    dw SerByName   ;功能3:按姓名搜索
    dw Sort     ;功能4:按成绩排序
    dw Range     ;功能5:显示分数段
    dw Exit     ;功能6:退出系统

loopS:
    ;打印菜单
    output menu 
    output choice
    
    ;输入选择
    mov ah,01h
    int 21h
    sub al,30h      ;ASCII转数字
    
    ;验证输入范围(1-6)
    cmp al,1
    jb exit0         ;<1
    cmp al,6
    ja exit0         ;>6
    
    mov bl,al       ;保存输入值
    
    call new_line    ;换行
    
    ;使用跳转表实现功能跳转
    mov bh,0
    dec bl           ;功能号减1（0-5）
    shl bl,1        ;乘2（每个地址2字节）
    mov si,bx       ;将索引存入si
    jmp word ptr [func_table+si] ;跳转到相应的功能

exit0:
    output error0    ;显示错误信息
    jmp loopS        ;重新输入

;输入功能
input:
    push_all
    output how_much_student
    
    ;输入学生数量
    call input_number      ;输入数字
    mov bl,al             ;学生数量保存在bl

    
    ;检查数量范围(1-20)
    cmp bl,1
    jb input_error
    cmp bl,max_students
    ja input_error
    
    jmp input_count_ok
    
input_error:
    output error0
    pop_all
    jmp input
    
input_count_ok:
    mov current_count,bl ;保存学生数量
    mov bh,0        ;当前学生计数器 (初始化为0)
    mov si,offset student_info
    
input_loop:
    ;输入姓名（enter提前结束）
    output input_name
    mov di,si
    mov cx,5
    call input_string_with_enter
    
    ;输入学号（enter提前结束）
    output input_id
    mov di,si
    add di,5        ;5(name)
    mov cx,8
    call input_string_with_enter
    
    ;输入8次作业成绩
    output input_scores
    mov di,si
    add di,13       ;5(name)+8(id)=13
    mov cx,8			;循环8次
    call input_scores_sub
    
    ;输入大作业成绩(bw)
    output input_bw
    mov di,si
    add di,21       ;5(name)+8(id)+8(scores)=21
    mov cx,1
    call input_scores_sub
    
    ;计算最终成绩(fi)
    call calculate_final_grade
    
    ;处理下一个学生
    add si,student_size  ;每个学生24字节
    inc bh           ;学生计数器+1
    cmp bh,bl
    jb input_loop    ;继续输入直到达到指定数量
    
    ;所有学生输入完成后，计算排名
    call calculate_rankings
    
; 计算最高分、最低分和平均分
; 初始化统计值
mov al, 0          ; 最高分初始为0
mov bl, 100        ; 最低分初始为100
xor dx, dx         ; 总分累加器清零

mov cl, current_count
mov ch, 0
jcxz no_stats      ; 如果学生数为0则跳过

mov si, offset student_info

calc_stats_loop:
    mov al, [si+22]       ; 获取当前学生的最终成绩（fi）
    

    
    ; 更新最高分
    cmp al, ah
    jbe not_higher_calc
    mov ah, al
    
not_higher_calc:
    ; 更新最低分
    cmp al, bl
    jae not_lower_calc
    mov bl, al
    
not_lower_calc:
    ; 累加到总分（使用保存的分数值
	push ax
    xor ah, ah            ; 清零AH
    add dx, ax            ; 累加当前分数到总分
    
    ; 移动到下一个学生
    add si, student_size
	pop ax
    loop calc_stats_loop
    
    ; 存储最高分和最低分
    mov highest_score, ah
    mov lowest_score, bl
    
    ; 计算平均分
    mov ax, dx            ; 总分
    mov cl, current_count
    xor ch, ch
    div cl                ; AL = 总分/学生数（商在AL，余数在AH）
    ; 存储平均分
    mov average_score, al
    
    jmp end_calc_stats
    
no_stats:
    ; 没有学生数据，清零
    mov highest_score, 0
    mov lowest_score, 0
    mov average_score, 0
    
end_calc_stats: 
    pop_all
    jmp loopS

;按学号搜索
SerByID:

    push_all
    
    ;检查是否有学生数据
    cmp byte ptr current_count,0
    jne has_student_data_2
    jmp no_student_data
    
has_student_data_2:
    ;提示输入学号
    lea dx,input_id
    mov ah,09h
    int 21h
    
    ;输入学号到缓冲区
    mov di,offset search_buffer
    mov cx,8
    call input_string_with_enter
    
    ;设置搜索参数
    mov cx,0
    mov cl,current_count  ;学生数量
    mov si,offset student_info

search_by_id_loop:
    push si
    push cx         ;保存cx
    
    add si,5       ;指向ID字段
    mov di,offset search_buffer
    mov cx,8       ;设置比较次数
    repe cmpsb      ;比较ID
    
    pop cx          ;恢复cx
    pop si
    
    je found_student_ID ;匹配成功
    
    ;切换到下一个学生
    add si,student_size  ;每个学生24字节
    loop search_by_id_loop
    
    ;未找到
    output not_found_msg
    jmp search_done_ID

found_student_ID:
    ;显示学生信息
	output student_header
    call display_student_info
    
search_done_ID:
	call new_line
    pop_all
    jmp loopS

SerByName:
    ;按姓名搜索
    push_all
    
    ;检查是否有学生数据
    cmp byte ptr current_count,0
    jne has_student_data_3
    jmp no_student_data
    
has_student_data_3:
    ;提示输入姓名
    lea dx,input_name
    mov ah,09h
    int 21h
    
    ;输入姓名到缓冲区
    mov di,offset search_buffer
    mov cx,5
    call input_string_with_enter
    
    ;设置搜索参数
    mov cx,0
    mov cl,current_count  ;学生数量
    mov si,offset student_info
    
search_by_name_loop:
    push si
    push cx          ;保存cx寄存器
    
    mov di,offset search_buffer
    mov cx,5        ;设置姓名比较次数为5
    repe cmpsb       ;逐字节比较姓名
    
    pop cx           ;恢复cx寄存器
    pop si
    
    je found_student_Name ;匹配成功则跳转
    
    ;切换到下一个学生
    add si,student_size  ;每个学生24字节
    loop search_by_name_loop
    
    ;未找到
    output not_found_msg
    jmp search_done_Name

found_student_Name:
    ;显示学生信息
	output student_header
    call display_student_info
    
search_done_Name:
	call new_line
    pop_all
    jmp loopS

Sort:
    ;按成绩排序（升序）
    push_all
    
    ;检查是否有学生数据
    cmp byte ptr current_count,0
    jne has_student_data_4
    jmp no_student_data
    
has_student_data_4:
    ;使用冒泡排序（升序）
    mov cl,current_count
    dec cl          ;外循环次数 = n-1
    jz sort_done    ;如果只有一个学生，不需要排序
    
outer_loop:
    mov si,offset student_info
    mov dx,cx      ;内循环次数
    
inner_loop:
    ;比较两个学生的最终成绩（升序）
    mov al,[si+22]   ;学生1的fi
    mov bx,si
    add bx,student_size
    cmp al,[bx+22]   ;学生2的fi
    jbe no_swap      ;如果学生1 <= 学生2，不交换（升序）
    
    ;交换两个学生记录
    push cx
    mov cx,student_size  ;每个学生24字节
    mov di,si
    add di,student_size  ;指向下一个学生
    
swap_loop:
    mov al,[si]
    mov ah,[di]
    mov [si],ah
    mov [di],al
    inc si
    inc di
    loop swap_loop
    pop cx
    
no_swap:
    mov si,bx       ;移动到下一个学生
    dec dx
    jnz inner_loop
    
    loop outer_loop
    
sort_done:
    ;重新计算排名
    call calculate_rankings
    
    ;显示所有学生
    call display_all_students
	call new_line
    pop_all
    jmp loopS

;分数段、最高分最低分平均分显示
Range:
    push_all
    
    ;检查是否有学生数据
    cmp byte ptr current_count,0
    jne has_student_data_5
    jmp no_student_data
    
has_student_data_5:
    ;初始化分数段计数器
    mov di,offset score_ranges
    mov cx,5
    xor ax,ax
    rep stosb
    
    ;统计分数段
    mov cl,current_count
    mov si,offset student_info
    
score_range_loop:
    mov al,[si+22] ;获取最终成绩(fi)
    
    cmp al,90
    jb below_90
    inc byte ptr [score_ranges] ;90-100
    jmp next_student
    
below_90:
    cmp al,80
    jb below_80
    inc byte ptr [score_ranges+1] ;80-89
    jmp next_student
    
below_80:
    cmp al,70
    jb below_70
    inc byte ptr [score_ranges+2] ;70-79
    jmp next_student
    
below_70:
    cmp al,60
    jb below_60
    inc byte ptr [score_ranges+3] ;60-69
    jmp next_student
    
below_60:
    inc byte ptr [score_ranges+4] ;0-59
    
next_student:
    add si,student_size  ;每个学生24字节
    dec cl
    jnz score_range_loop
    
    ;显示分数段统计
    output score_range_header
    
    ;90-100
    output range_90_100
    mov al,[score_ranges]
    call print_number
    call new_line
    
    ;80-89
    output range_80_89
    mov al,[score_ranges+1]
    call print_number
    call new_line
    
    ;70-79
    output range_70_79
    mov al,[score_ranges+2]
    call print_number
    call new_line
    
    ;60-69
    output range_60_69
    mov al,[score_ranges+3]
    call print_number
    call new_line
    
    ;0-59
    output range_0_59
    mov al,[score_ranges+4]
    call print_number
    call new_line

    ;显示最高分
    output highest_msg
    mov al,highest_score
    call print_score
    
    ;显示最低分
    output lowest_msg
    mov al,lowest_score
    call print_score
    
    ;显示平均分
    output average_msg
    mov al,average_score
    call print_score
    call new_line
    
    pop_all
    jmp loopS

Exit:
    mov ah,4ch
    int 21h

no_student_data:
    output no_student_msg
    pop_all
    jmp loopS

;--------------子程序-------------

;显示所有学生信息
display_all_students proc
    push_all
    
    ;检查是否有学生数据
    cmp byte ptr current_count,0
    je display_done
    
    ;显示表头
    output student_header
    
    ;遍历所有学生
    mov cl,current_count
    mov si,offset student_info
    
display_loop:
    call display_student_info
    call new_line
    add si,student_size  ;每个学生24字节
    dec cl
    jnz display_loop
    
display_done:
    pop_all
    ret
display_all_students endp

;显示单个学生信息
display_student_info proc
    push_all
    
    ;显示姓名
    mov cx,5
    mov di,si
display_name:
    mov dl,[di]
    cmp dl,'$'     ;检查结束符
    je name_done
    mov ah,02h
    int 21h
    inc di
    loop display_name
    
name_done:
    print_space
    
    ;显示学号
    add si,5
    mov cx,8
    mov di,si
display_id:
    mov dl,[di]
    cmp dl,'$'     ;检查结束符
    je id_done
    mov ah,02h
    int 21h
    inc di
    loop display_id
    
id_done:
    print_space
    sub si,5       ;恢复si到起始位置
    
    ;显示8次平时作业成绩
    mov cx,8
    mov di,si
    add di,13      ;成绩开始位置
    
display_scores:
    print_space
    mov al,[di]    ;将成绩值加载到al寄存器
    push cx         ;保存循环计数器
    call print_score  ;调用新的打印成绩过程
    pop cx          ;恢复循环计数器
    inc di
    loop display_scores
    
    print_space
    print_space
    
    ;显示大作业成绩(bw)
    mov al,[si+21]
    call print_score
    
    print_space
    print_space
    
    ;显示最终成绩(fi)
    mov al,[si+22]
    call print_score
    
    print_space
    print_space
    
    ;显示排名
    mov al,[si+23] ;排名在第24个字节
    call print_score
    
    pop_all
    ret
display_student_info endp

;打印成绩（0-100）

print_score proc
    push ax
    push bx
    push cx
    push dx
    
    ;检查是否为100
    cmp al,100
    je print_100
    
    mov ah,0
    ;除以10，拆分十位和个位
    mov bl,10
    div bl      ;al=十位,ah=个位
    
    mov bh,ah  ;将个位数存入bh寄存器，防止输出十位数时覆盖
    
    ;检查十位是否为0
    cmp al,0
    je print_one_digit
    
    ;打印两位数
    add al,'0'    ;十位转ASCII
    mov dl,al
    mov ah,02h
    int 21h        ;显示十位
    
    ;恢复个位数（从bh取回）
    mov al,bh
    add al,'0'    ;个位转ASCII
    mov dl,al
    mov ah,02h
    int 21h        ;显示个位
    jmp print_done
    
print_one_digit:
    ;打印一位数
    mov dl,' '		;方便对齐
    mov ah,02h
    int 21h
    
    mov al,bh     ;从bh取回个位数
    add al,'0'
    mov dl,al
    mov ah,02h
    int 21h
    jmp print_done
    
print_100:
    mov dl,'1'
    mov ah,02h
    int 21h
    mov dl,'0'
    int 21h
    mov dl,'0'
    int 21h
    
print_done:
    pop dx
    pop cx
    pop bx
    pop ax
    ret
print_score endp

new_line proc
    push dx
    push ax
    mov dl,0dh
    mov ah,02h
    int 21h
    mov dl,0ah
    int 21h
    pop ax
    pop dx
    ret
new_line endp

;计算最终成绩(fi)子程序
calculate_final_grade proc
    push_all
    
    ;计算平时成绩平均值(us)
    mov di,si
    add di,13       ;平时成绩起始位置
    mov cx,8
    xor ax,ax       ;清空ax用于累加
    
sum_loop:
    mov bl,[di]     ;获取成绩
    xor bh,bh
    add ax,bx       ;累加成绩
    inc di
    loop sum_loop
    
    ;计算平均值 (ax = 总和)
    mov bl,8
    div bl           ;al = 平均值,ah = 余数
    mov dl,al       ;保存作业成绩和
    
    ;获取大作业成绩(bw)
    mov dh,[si+21]  ;bw
    
    ;计算最终成绩fi= 0.4*作业成绩和 + 0.6*bw
    ;计算4*作业成绩和
    xor ah,ah       ;清除ah
    mov al,dl       ;作业成绩和
    mov cl,4
    mul cl           ;ax=us*4
    mov bx,ax       ;保存结果到bx
    
    ;计算6*bw
    xor ah,ah
    mov al,dh
    mov cl,6
    mul cl
    
    ;相加除以10
    add ax,bx
    mov cl,10
    div cl           ;al = 商 (最终成绩
    
    mov [si+22],al  ;保存最终成绩(fi)
    
    pop_all
    ret
calculate_final_grade endp

;计算排名子程序
calculate_rankings proc
    push_all
    
    ;获取学生数量
    mov cl,current_count
    cmp cl,0
    je end_ranking
    
    ;初始化所有学生的排名为1
    mov si,offset student_info
    mov di,si
    mov al,1
init_rank_loop:
    mov [di+23],al  ;设置初始排名
    add di,student_size  ;下一个学生
    dec cl
    jnz init_rank_loop
    
    ;比较所有学生，计算排名
    mov cl,current_count
    dec cl                       ;外层循环次数（学生数-1）
    jz end_ranking               ;如果只有一个学生，跳过比较
    
    mov si,offset student_info  ;外层循环指针
    
outer_ranking_loop:
    mov di,si                   ;内层循环指针
    add di,student_size         ;下一个学生
    mov ch,cl                   ;保存内层循环次数
    
inner_ranking_loop:
    ;比较两个学生的最终成绩(fi)
    mov al,[si+22]   ;当前学生的fi
    cmp al,[di+22]   ;比较学生的fi
    jae not_lower
    
    ;如果当前学生成绩较低，增加其排名
    inc byte ptr [si+23]  ;增加当前学生排名
    jmp check_lower
    
not_lower:
    ;如果比较学生成绩较低，增加其排名
    mov al,[di+22]
    cmp al,[si+22]
    jae not_lower2
    inc byte ptr [di+23]  ;增加比较学生排名
    
not_lower2:
check_lower:
    add di,student_size  ;下一个比较学生
    dec ch
    jnz inner_ranking_loop   ;继续内层比较
    
    add si,student_size     ;下一个当前学生
    loop outer_ranking_loop  ;继续外层循环
    
end_ranking:
    pop_all
    ret
calculate_rankings endp

;显示分数段人数
;打印数字
print_number proc
    push ax
    push dx
    aam             ;将al转换为BCD码(ah=十位,al=个位)
    mov dl,ah
    cmp dl,0
    je skip_tens
    add dl,'0'
    mov ah,02h
    int 21h
    
skip_tens:
    mov dl,al
    add dl,'0'
    mov ah,02h
    int 21h
    
    pop dx
    pop ax
    ret
print_number endp

;输入字符串函数
input_string_with_enter proc
    push ax
    push cx
    push di
    push si
    
    ;保存原始位置
    mov si,di
    
    ;保存最大长度
    push cx
    
    ;初始化缓冲区为空格
    mov al,' '
    rep stosb
    
    ;恢复最大长度到cx
    pop cx
    mov di,si       ;恢复di到起始位置
    
input_char:
    mov ah,01h
    int 21h
    
    ;检查回车键（提前结束）
    cmp al,0Dh
    je fill_remaining
    
    ;检查退格键
    cmp al,08h
    je handle_backspace
    
    ;存储字符
    mov [di],al
    inc di
    dec cx
    jnz input_char   ;继续输入直到达到最大长度
    
    jmp input_done

handle_backspace:
    ;如果已经在开始位置，忽略退格
    cmp di,si
    jbe input_char
    
    ;执行退格操作
    dec di
    inc cx
    
    ;在屏幕上显示退格效果
    push dx
    mov ah,02h
    mov dl,' '     ;空格
    int 21h
    mov dl,08h     ;再次退格
    int 21h
    pop dx
    
    jmp input_char

fill_remaining:
    ;用空格填充剩余部分
    cmp cx,0
    je input_done
    mov al,' '
fill_loop:
    mov [di],al
    inc di
    loop fill_loop

input_done:
    ;添加结束符
    mov byte ptr [di],'$'
    
    pop si
    pop di
    pop cx
    pop ax
    ret
input_string_with_enter endp

;成绩输入子程序
input_scores_sub proc
    push ax
    push bx
    push cx
    push dx
    push di
    
    mov cx,cx      ;成绩个数
    mov dx,di      ;保存起始位置
    
score_loop:
    ;为每个成绩重置bx
    xor bx,bx      ;清空bx用于存储成绩值
    
get_digit:
    mov ah,01h     ;读取一个字符
    int 21h
    
    ;检查回车键
    cmp al,0Dh
    je process_score
    
    ;检查空格键
    cmp al,' '
    je process_score
    
    ;检查是否数字
    cmp al,'0'
    jb invalid_char
    cmp al,'9'
    ja invalid_char
    
    ;将ASCII转换为数字
    sub al,'0'
    xor ah,ah
    
    ;bx=bx*10+ax
    push ax
    mov ax,bx
    mov bx,10
    mul bx          ;ax=ax*10
    mov bx,ax
    pop ax
    add bx,ax
    
    jmp get_digit   ;获取下一个字符

invalid_char:
    output error0
    jmp get_digit

process_score:
    ; 检查成绩范围 (0-100)
    cmp bx, 0
    jl score_too_low
    cmp bx, 100
    ja score_too_high
    
    ; 保存成绩
    mov [di], bl
    inc di          ; 移动到下一个成绩位置
    
    ; 显示空格分隔符（仅限平时成绩）
    cmp cx, 1
    je no_space
    print_space
no_space:
    
    dec cx          ; 减少剩余成绩计数
    jz input_done_score ; 如果所有成绩都输入完成
    
    ; 继续输入下一个成绩
    jmp score_loop

score_too_low:
score_too_high:
    output error_range
    ; 重新输入当前成绩
    jmp score_loop

input_done_score:
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret
input_scores_sub endp

;输入数字子程序（1-20）
input_number proc
    push bx
    push cx
    push dx
    
    xor bx,bx      ;清空bx，用于存储数字
    
input_digit1:
    mov ah,01h     ;读取一个字符
    int 21h
    
    cmp al,0Dh     ;检查回车键
    je input_done_num
    
    ;检查是否数字
    cmp al,'0'
    jb invalid_input_num
    cmp al,'9'
    ja invalid_input_num
    
    ;将ASCII转换为数字
    sub al,'0'
    mov bl,al      ;保存第一位数字
    
    ;读取第二位数字（可能没有）
    mov ah,01h
    int 21h
    
    cmp al,0Dh     ;如果是回车，则只有一位数
    je one_digit
    
    ;检查第二位是否数字
    cmp al,'0'
    jb invalid_input_num
    cmp al,'9'
    ja invalid_input_num
    
    sub al,'0'     ;转换为数字
    mov cl,al      ;保存第二位数字
    
    ;计算两位数：bl=bl*10+cl
    mov al,bl
    mov ah,10
    mul ah          ;ax=al*10
    add al,cl
    mov bl,al      ;保存结果
    jmp input_done_num

one_digit:
    ;只有一位数，BL已经是结果
    mov al,bl
    jmp input_done_num_store

invalid_input_num:
    ;显示错误并重新输入
    output error0
    jmp input_number

input_done_num:
    mov al,bl      ;结果存入al

input_done_num_store:
    pop dx
    pop cx
    pop bx
    ret
input_number endp

code ends
end start