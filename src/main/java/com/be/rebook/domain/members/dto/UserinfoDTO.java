package com.be.rebook.domain.members.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Builder
@Getter
public class UserinfoDTO {
    private String nickname;
    private String university;
    private String majors;
}
