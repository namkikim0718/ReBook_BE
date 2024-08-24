package com.be.rebook.members.dto;

import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class OtherUserinfoDTO {
    private Long memberId;
    private String username;
    private String nickname;
    private String university;
    private String majors;
    private String storedFileName;
}
