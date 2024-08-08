package com.be.rebook.auth.controller;


import com.be.rebook.auth.entity.Members;
import com.be.rebook.auth.entity.RefreshTokens;
import com.be.rebook.auth.dto.SignupDTO;
import com.be.rebook.auth.service.ReissueService;
import com.be.rebook.auth.service.SignupService;
import com.be.rebook.common.config.BaseResponse;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;

import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


import org.slf4j.Logger;

@RestController
@RequestMapping("/auth")
public class AuthController {

    private static final Logger signupLogger = LoggerFactory.getLogger(AuthController.class);
    private final SignupService signupService;

    private final ReissueService reissueService;

    public AuthController(SignupService signupService,
                          ReissueService reissueService){
        this.signupService = signupService;
        this.reissueService = reissueService;
    }

    @PostMapping("/members/signup")
    public ResponseEntity<BaseResponse<Members>> signupProcess(@Valid @RequestBody SignupDTO signupDTO) {
        signupLogger.info(signupDTO.getUsername());
        return ResponseEntity.ok().body(new BaseResponse<>(signupService.signupProcess(signupDTO)));
    }

    @PostMapping("/members/refreshtoken")
    public ResponseEntity<BaseResponse<RefreshTokens>> reissue(HttpServletRequest request, HttpServletResponse response) {
        return ResponseEntity.ok().body(new BaseResponse<>(reissueService.reissueToken(request, response)));
    }
}