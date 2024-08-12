package com.be.rebook.members.dto;

import jakarta.validation.constraints.Pattern;
import lombok.Getter;
import lombok.Setter;
@Setter
@Getter
public class UpdateDTO {
    private String nickname;
    
    private String university;

    @Pattern(regexp = ".*[^a-zA-Z0-9,\\uAC00-\\uD7AF\\u4E00-\\u9FFF ()\\u00B7-].*", message = "전공 입력이 잘못되었습니다.")
    private String majors;
}
