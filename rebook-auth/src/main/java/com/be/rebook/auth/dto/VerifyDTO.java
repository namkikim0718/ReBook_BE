package com.be.rebook.auth.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class VerifyDTO{
    @NotNull(message = "Username cannot be null")
    @Size(min = 3, max = 50, message = "Username must be between 3 and 50 characters")
    @Pattern(regexp = "^[a-zA-Z0-9.@]*$", message = "Username can only contain alphanumeric characters")
    @Email(message = "Invalid email format")
    private String username;

    @Size(min = 6, max = 6, message = "6자리 숫자만 입력 가능합니다.")
    private String code;
}
