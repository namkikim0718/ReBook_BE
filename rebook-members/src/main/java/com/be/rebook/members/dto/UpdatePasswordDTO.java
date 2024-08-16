package com.be.rebook.members.dto;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.Getter;

@Getter
public class UpdatePasswordDTO {
    @NotNull(message = "Password cannot be null")
    @Size(min = 8, max = 100, message = "Password must be between 8 and 100 characters")
    @Pattern(regexp = "^[a-zA-Z0-9@#$%^&+=]*$", message = "Password can only contain alphanumeric characters and '@','#','$','%','^','&','+','='")
    private String password;
}
