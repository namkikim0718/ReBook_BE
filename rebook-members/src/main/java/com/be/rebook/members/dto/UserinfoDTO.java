package com.be.rebook.members.dto;

import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
public class UserinfoDTO {
    private String username;
    private String nickname;
    private String university;
    private String majors;
    private String storedFileName;
}
