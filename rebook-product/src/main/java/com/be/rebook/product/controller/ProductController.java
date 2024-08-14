package com.be.rebook.product.controller;

import com.be.rebook.common.argumentresolver.auth.Auth;
import com.be.rebook.common.argumentresolver.auth.MemberLoginInfo;
import com.be.rebook.common.config.BaseResponse;
import com.be.rebook.product.service.ProductService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.List;

import static com.be.rebook.product.dto.ProductRequestDTO.*;
import static com.be.rebook.product.dto.ProductResponseDTO.*;


@RestController
@RequiredArgsConstructor
@RequestMapping("/products")
public class ProductController {

    private final ProductService productService;

    /**
     * 상품 등록
     */
    @PostMapping("/me") // TODO:유저정보는 Auth 모듈 활용해서 가져오기
    public ResponseEntity<BaseResponse<Long>> createProduct(@Auth MemberLoginInfo memberLoginInfo,
                                                            @RequestPart("productRequest") ProductSaveRequestDTO productSaveRequestDTO,
                                                            @RequestPart("imageFiles") List<MultipartFile> imageFiles) throws IOException {
        return ResponseEntity.status(HttpStatus.CREATED).body(new BaseResponse<>(
                HttpStatus.CREATED,
                "201 CREATED",
                "상품이 등록되었습니다.",
                productService.createProduct(memberLoginInfo.getUsername(), productSaveRequestDTO, imageFiles)));
    }

    /**
     * 상품 목록 조회
     */
    @GetMapping
    public ResponseEntity<BaseResponse<List<ProductListResponseDTO>>> findAllProduct(@Valid @ModelAttribute ProductFilterDTO productFilterDTO) {
        return ResponseEntity.ok().body(new BaseResponse<>(productService.findAllProductByFilter(productFilterDTO)));
    }

    /**
     * 내가 쓴 글 조회
     */
    @GetMapping("/me")
    public ResponseEntity<BaseResponse<List<ProductListResponseDTO>>> findProductsByMember(@Auth MemberLoginInfo memberLoginInfo) {
        return ResponseEntity.ok().body(new BaseResponse<>(productService.findAllMyProducts(memberLoginInfo.getUsername())));
    }

    /**
     * 상품 단건 조회
     */
    @GetMapping("/{productId}")
    public ResponseEntity<BaseResponse<ProductDetailResponseDTO>> findProductById(@PathVariable("productId") Long productId) {
        return ResponseEntity.ok().body(new BaseResponse<>(productService.findProductById(productId)));
    }

    /**
     * 상품 상태 변경
     */
    @PatchMapping("/{productId}")
    public ResponseEntity<BaseResponse<Long>> changeProductStatus(@PathVariable("productId") Long productId,
                                                                  @RequestBody ProductStatusRequestDTO productStatusRequestDTO) {
        return ResponseEntity.ok().body(new BaseResponse<>(productService.changeStatus(productId, productStatusRequestDTO.getStatus())));
    }

    /**
     * 상품 삭제
     */
    @DeleteMapping("/{productId}")
    public ResponseEntity<BaseResponse<String>> deleteProductById(@PathVariable("productId") Long productId) {
        return ResponseEntity.ok().body(new BaseResponse<>(productService.deleteById(productId)));
    }

}
