package com.be.rebook.auth.dto;

import jakarta.validation.constraints.Size;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class VerifyDTO extends JoinDTO{
    @Size(min = 6, max = 6, message = "6자리 숫자만 입력 가능합니다.")
    private String code;
}
