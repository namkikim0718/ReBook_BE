package com.be.rebook.auth.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class SignupDTO extends BasicUserInfoDTO{
    private String mailauth;
}
